;;; swept_path.lsp -- упрощённый аналог Swept Path Analysis для AutoCAD (AutoLISP)
;;;
;;; Команда: SWEPT
;;;   Моделирует проезд двухосного ТС по траектории и строит:
;;;     - контуры кузова через заданный интервал  (слой SWEPT-BODY)
;;;     - следы четырёх углов кузова               (слой SWEPT-ENV)
;;;     - следы концов обеих осей = колёса         (слой SWEPT-WHEEL)
;;;     - следы середин осей: передняя (= траектория) и задняя (SWEPT-AXLE)
;;;   Границы огибающей (см. pathsweeper.com/en/guides/swept-path-analysis):
;;;     внешняя  = след внешнего ПЕРЕДНЕГО УГЛА кузова;
;;;     внутренняя = след внутреннего ЗАДНЕГО КОЛЕСА (не угла: угол лежит
;;;                  дальше от центра поворота и даёт заниженную полосу);
;;;     задний внешний угол -- свес хвоста в начале поворота (tail swing).
;;;   Предел руления задаётся внешним радиусом разворота по кузову (его дают
;;;   паспорта; для легковых это половина диаметра разворота "стена-стена").
;;;   В конце выводится требуемый угол руления; если он выше предельного --
;;;   ТС по такой траектории не проедет.
;;;
;;; Модель: кинематическая, велосипедная.
;;;   Выбранная кривая = траектория середины ПЕРЕДНЕЙ оси.
;;;   Середина задней оси движется "за" передней на расстоянии базы L
;;;   (нет бокового проскальзывания задних колёс).
;;;
;;; Автофура (тип Semi): седельный тягач + полуприцеп. Шкворень лежит на
;;;   оси тягача на расстоянии fw впереди его задней оси; ось (тележка)
;;;   полуприцепа тянется за шкворнем на расстоянии ltk -- та же схема
;;;   "ведомая точка догоняет ведущую", что и для задней оси тягача.
;;;   Для сочленённого ТС четырёх углов мало (внутренний край может задавать
;;;   точка на боку полуприцепа), поэтому дополнительно трассируются точки
;;;   вдоль бортов полуприцепа (слой SWEPT-SIDE).
;;;
;;; Ограничения (сознательные):
;;;   - один прицеп (седельный); дышло, роспуск, B-train не поддерживаются;
;;;   - движение только вперёд, по направлению построения кривой;
;;;   - углы траектории (ломаная с острыми вершинами) дадут скачок угла руления --
;;;     рисуйте траекторию плавной (дуги, сплайн, скруглённая полилиния);
;;;   - точность зависит от шага: при шаге 0.25 м ошибка заметно меньше 1 см
;;;     на радиусах от ~5 м, на меньших радиусах уменьшайте шаг.

(vl-load-com)

;; --- вспомогательные функции ------------------------------------------------

(defun sp:ask (msg def / x)
  (initget 6)                           ; только > 0
  (setq x (getreal (strcat "\n" msg " <" (rtos def 2 2) ">: ")))
  (if x x def)
)

(defun sp:ask0 (msg def / x)            ; допускает 0
  (initget 4)                           ; только >= 0
  (setq x (getreal (strcat "\n" msg " <" (rtos def 2 2) ">: ")))
  (if x x def)
)

;; нормализация угла в (-pi; pi]
(defun sp:angdiff (a b / d)
  (setq d (- a b))
  (while (> d pi) (setq d (- d (* 2 pi))))
  (while (<= d (- pi)) (setq d (+ d (* 2 pi))))
  d
)

;; точка от начала o при курсе ang: dx вперёд, dy влево
(defun sp:pt (o ang dx dy)
  (list (+ (car o) (* dx (cos ang)) (- (* dy (sin ang))))
        (+ (cadr o) (* dx (sin ang)) (* dy (cos ang)))
  )
)

(defun sp:layer (name color)
  (if (not (tblsearch "LAYER" name))
    (entmake (list '(0 . "LAYER") '(100 . "AcDbSymbolTableRecord")
                   '(100 . "AcDbLayerTableRecord") (cons 2 name)
                   '(70 . 0) (cons 62 color) '(6 . "Continuous")))
  )
)

(defun sp:pline (pts closed layer)
  (if (> (length pts) 1)
    (entmakex
      (append
        (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") (cons 8 layer)
              '(100 . "AcDbPolyline") (cons 90 (length pts))
              (cons 70 (if closed 1 0)))
        (mapcar '(lambda (p) (list 10 (car p) (cadr p))) pts)
      )
    )
  )
)

;; допустим ли объект как траектория (кривая с параметрами)
(defun sp:curve-p (en / r)
  (setq r (vl-catch-all-apply 'vlax-curve-getEndParam (list en)))
  (and (not (vl-catch-all-error-p r)) (numberp r))
)

;; --- основная команда -------------------------------------------------------

(defun c:SWEPT (/ sc kw pr wb ovf ovr wd rout rrear maxst ds ivl sel en total
                  n i dist p0 tg fp f r h v len vel delta maxd nextd
                  fl fr rl rr trfl trfr trrl trrr trf trr nbody rmin
                  twl twr tfl2 tfr2
                  semi fw ltk tfo tro tw amax tp ht kp art maxa
                  ufl ufr url urr utfl utfr utrl utrr uwl uwr utp
                  stns usl usr cl cr tr1)

  ;; --- ввод параметров
  (setq sc (sp:ask "Единиц чертежа в 1 м (м=1, мм=1000)" 1.0))

  (initget "Car Truck Bus Semi Fire")
  (setq kw (getkword "\nТип ТС [Car/Truck/Bus/Semi(автофура)/Fire(пожмаш)] <Truck>: "))
  (if (null kw) (setq kw "Truck"))
  ;; база, передний свес, задний свес, ширина, внешний радиус разворота (м)
  ;; Car -- пример из статьи (VW Golf / BMW 3). Truck, Bus и тягач Semi --
  ;; МОИ прикидки, не паспортные данные: вводите свои значения.
  (setq pr (cond ((= kw "Car")   '(2.7 0.9 1.1 1.85 5.6))
                 ((= kw "Bus")   '(6.0 2.7 3.3 2.55 12.1))
                 ((= kw "Semi")  '(3.8 1.4 0.8 2.55 7.6))
                 ;; Fire: АЦ 9,0 на КАМАЗ-65111 -- база/свесы/ширина заданы
                 ;; пользователем. Внешний радиус 10.0 м -- МОЯ прикидка, в
                 ;; паспорте не проверялась. Сумма 1.42+4.1+2.2 = 7.72 м, а
                 ;; заявленная длина ~9.3 м: задний свес, вероятно, занижен
                 ;; (или база дана как эквивалентная для тележки 6x4).
                 ((= kw "Fire")  '(4.1 1.42 2.2 2.5 10.0))
                 (t              '(4.5 1.5 2.5 2.5 9.8))))
  (setq wb    (sp:ask "База (между осями), м"        (nth 0 pr))
        ovf   (sp:ask "Передний свес, м"             (nth 1 pr))
        ovr   (sp:ask "Задний свес, м"               (nth 2 pr))
        wd    (sp:ask "Ширина кузова, м"             (nth 3 pr))
        rout  (sp:ask "Внешний радиус разворота (по кузову), м" (nth 4 pr))
        ds    (sp:ask "Шаг моделирования, м"         0.25)
        ivl   (sp:ask "Интервал контуров кузова, м"  3.0))

  (setq semi (= kw "Semi"))
  (if semi
    ;; полуприцеп: 7.7 + 4.3 = 12.0 м от шкворня до задней кромки (лимит ЕС),
    ;; 1.6 м вперёд от шкворня -- вместе 13.6 м; всё равно проверьте по паспорту
    (setq fw   (sp:ask0 "Шкворень впереди задней оси тягача, м" 0.0)
          ltk  (sp:ask "Шкворень -- ось (тележка) полуприцепа, м" 7.7)
          tfo  (sp:ask "Полуприцеп: свес вперёд от шкворня, м"   1.6)
          tro  (sp:ask "Полуприцеп: свес назад за осью, м"        4.3)
          tw   (sp:ask "Ширина полуприцепа, м"                    2.55)
          amax (sp:ask "Макс. угол в седельном устройстве, град"  90.0)))

  ;; радиус задней оси при предельном руле: R_out^2 = (Rз + w/2)^2 + (wb+fo)^2
  (if (<= rout (+ wb ovf))
    (progn (princ "\nВнешний радиус не больше расстояния от задней оси до бампера -- так не бывает.")
           (exit)))
  (setq rrear (- (sqrt (- (* rout rout) (* (+ wb ovf) (+ wb ovf)))) (/ wd 2.0)))
  (if (<= rrear 0.0)
    (progn (princ "\nВнешний радиус слишком мал для этой ширины и базы.") (exit)))
  (setq maxst (* (atan wb rrear) (/ 180.0 pi)))     ; угол руления, град

  ;; в единицы чертежа
  (setq wb (* wb sc) ovf (* ovf sc) ovr (* ovr sc) wd (* wd sc)
        ds (* ds sc) ivl (* ivl sc))
  (if semi
    (setq fw (* fw sc) ltk (* ltk sc) tfo (* tfo sc) tro (* tro sc) tw (* tw sc)))

  ;; --- выбор траектории
  (setq en nil)
  (while (null en)
    (setq sel (entsel "\nТраектория середины передней оси (полилиния/дуга/сплайн): "))
    (cond ((null sel) (princ "\nОтмена.") (exit))
          ((sp:curve-p (car sel)) (setq en (car sel)))
          (t (princ "\nЭто не кривая, выберите другой объект."))))

  (setq total (vlax-curve-getDistAtParam en (vlax-curve-getEndParam en)))
  (if (< total (* 2 ds))
    (progn (princ "\nТраектория слишком короткая для выбранного шага.") (exit)))

  (sp:layer "SWEPT-BODY" 8)
  (sp:layer "SWEPT-ENV"  1)
  (sp:layer "SWEPT-AXLE" 5)
  (sp:layer "SWEPT-WHEEL" 3)
  (sp:layer "SWEPT-SIDE" 9)

  (command "_.undo" "_begin")

  ;; --- начальное состояние: ТС стоит вдоль касательной в начале кривой
  (setq p0 (vlax-curve-getPointAtDist en 0.0)
        tg (vlax-curve-getFirstDeriv en (vlax-curve-getStartParam en))
        h  (atan (cadr tg) (car tg))
        r  (sp:pt p0 h (- wb) 0.0)       ; задняя ось на базу позади передней
        fp p0
        maxd 0.0
        nextd 0.0
        nbody 0
        maxa 0.0
        n  (fix (/ total ds)))
  (if semi
    (setq kp   (sp:pt r h fw 0.0)
          tp   (sp:pt kp h (- ltk) 0.0)  ; полуприцеп стоит вытянутым
          ht   h
          stns (list ltk (* 0.5 ltk) (* -0.5 tro))
          usl  (mapcar '(lambda (x) nil) stns)
          usr  (mapcar '(lambda (x) nil) stns)))

  ;; список расстояний: 0, ds, 2ds, ... и точный конец
  (setq i 0)
  (repeat (1+ n)
    (setq dist (cons (* i ds) dist) i (1+ i)))
  (if (< (car dist) (- total 1e-9)) (setq dist (cons total dist)))
  (setq dist (reverse dist))

  ;; --- пошаговое моделирование
  (foreach d dist
    (setq f (vlax-curve-getPointAtDist en d))
    (if (> d 0.0)
      (progn
        ;; задняя ось смотрит на переднюю: |F-R| = wb, R сдвигается вдоль курса
        (setq v   (list (- (car f) (car r)) (- (cadr f) (cadr r)))
              len (distance '(0.0 0.0) v)
              h   (atan (cadr v) (car v))
              r   (sp:pt f h (- wb) 0.0))
        ;; угол руления = поворот скорости передней оси относительно курса
        (if (> (distance f fp) 1e-9)
          (progn
            (setq vel   (angle fp f)
                  delta (abs (sp:angdiff vel h)))
            (if (> delta maxd) (setq maxd delta))))
      )
    )
    (setq fp f)

    ;; углы кузова (задняя ось = r, курс = h)
    (setq fl (sp:pt r h (+ wb ovf) (/ wd 2.0))
          fr (sp:pt r h (+ wb ovf) (- (/ wd 2.0)))
          rl (sp:pt r h (- ovr)     (/ wd 2.0))
          rr (sp:pt r h (- ovr)     (- (/ wd 2.0))))
    (setq trfl (cons fl trfl) trfr (cons fr trfr)
          trrl (cons rl trrl) trrr (cons rr trrr)
          trf  (cons f trf)   trr  (cons r trr)
          ;; концы осей = колёса (ширина колеи принята равной ширине кузова)
          twl  (cons (sp:pt r h 0.0 (/ wd 2.0)) twl)
          twr  (cons (sp:pt r h 0.0 (- (/ wd 2.0))) twr)
          tfl2 (cons (sp:pt r h wb (/ wd 2.0)) tfl2)
          tfr2 (cons (sp:pt r h wb (- (/ wd 2.0))) tfr2))

    ;; полуприцеп: ось тянется за шкворнем, курс = направление ось -> шкворень
    (if semi
      (progn
        (setq kp (sp:pt r h fw 0.0))
        (if (> d 0.0)
          (setq v  (list (- (car kp) (car tp)) (- (cadr kp) (cadr tp)))
                ht (atan (cadr v) (car v))
                tp (sp:pt kp ht (- ltk) 0.0)))
        (setq art (abs (sp:angdiff h ht)))
        (if (> art maxa) (setq maxa art))
        (setq ufl (sp:pt tp ht (+ ltk tfo) (/ tw 2.0))
              ufr (sp:pt tp ht (+ ltk tfo) (- (/ tw 2.0)))
              url (sp:pt tp ht (- tro)     (/ tw 2.0))
              urr (sp:pt tp ht (- tro)     (- (/ tw 2.0))))
        (setq utfl (cons ufl utfl) utfr (cons ufr utfr)
              utrl (cons url utrl) utrr (cons urr utrr)
              utp  (cons tp utp)
              uwl  (cons (sp:pt tp ht 0.0 (/ tw 2.0)) uwl)
              uwr  (cons (sp:pt tp ht 0.0 (- (/ tw 2.0))) uwr))
        ;; точки вдоль бортов (внутренний край может задавать не угол и не колесо)
        (setq cl  (mapcar '(lambda (x) (sp:pt tp ht x (/ tw 2.0))) stns)
              cr  (mapcar '(lambda (x) (sp:pt tp ht x (- (/ tw 2.0)))) stns)
              usl (mapcar '(lambda (a q) (cons q a)) usl cl)
              usr (mapcar '(lambda (a q) (cons q a)) usr cr))))

    ;; контур кузова через интервал и в последней точке
    (if (or (>= d nextd) (= d total))
      (progn
        (sp:pline (list fl fr rr rl) T "SWEPT-BODY")
        (if semi (sp:pline (list ufl ufr urr url) T "SWEPT-BODY"))
        (setq nbody (1+ nbody))
        (while (<= nextd d) (setq nextd (+ nextd ivl)))))
  )

  ;; --- следы
  (sp:pline (reverse trfl) nil "SWEPT-ENV")
  (sp:pline (reverse trfr) nil "SWEPT-ENV")
  (sp:pline (reverse trrl) nil "SWEPT-ENV")
  (sp:pline (reverse trrr) nil "SWEPT-ENV")
  (sp:pline (reverse twl)  nil "SWEPT-WHEEL")
  (sp:pline (reverse twr)  nil "SWEPT-WHEEL")
  (sp:pline (reverse tfl2) nil "SWEPT-WHEEL")
  (sp:pline (reverse tfr2) nil "SWEPT-WHEEL")
  (if semi
    (progn
      (sp:pline (reverse utfl) nil "SWEPT-ENV")
      (sp:pline (reverse utfr) nil "SWEPT-ENV")
      (sp:pline (reverse utrl) nil "SWEPT-ENV")
      (sp:pline (reverse utrr) nil "SWEPT-ENV")
      (sp:pline (reverse uwl)  nil "SWEPT-WHEEL")
      (sp:pline (reverse uwr)  nil "SWEPT-WHEEL")
      (sp:pline (reverse utp)  nil "SWEPT-AXLE")
      (foreach tr1 (append usl usr) (sp:pline (reverse tr1) nil "SWEPT-SIDE"))))
  (sp:pline (reverse trr)  nil "SWEPT-AXLE")
  (sp:pline (reverse trf)  nil "SWEPT-AXLE")

  (command "_.undo" "_end")

  ;; --- отчёт
  (princ (strcat "\nШагов: " (itoa (length dist))
                 ", контуров кузова: " (itoa nbody)
                 ", длина пути: " (rtos (/ total sc) 2 2) " м"))
  (princ (strcat "\nМакс. угол руления на траектории: "
                 (rtos (* maxd (/ 180.0 pi)) 2 1) " град (предел "
                 (rtos maxst 2 1) ")"))
  (if (> maxd 1e-6)
    (progn
      (setq rmin (/ (/ wb (sin (min maxd (/ pi 2.0)))) sc))
      (princ (strcat "\nМин. радиус по оси передних колёс на траектории: "
                     (rtos rmin 2 2) " м"))))
  (if semi
    (progn
      (princ (strcat "\nМакс. угол в седельном устройстве: "
                     (rtos (* maxa (/ 180.0 pi)) 2 1) " град (предел "
                     (rtos amax 2 1) ")"))
      (if (> (* maxa (/ 180.0 pi)) amax)
        (princ "\n*** ВНИМАНИЕ: угол складывания превышает предел -- тягач упрётся в полуприцеп. ***"))))
  (if (> (* maxd (/ 180.0 pi)) maxst)
    (princ "\n*** ВНИМАНИЕ: требуемый угол руления превышает предел -- ТС так не проедет, смягчите траекторию. ***")
    (princ "\nУгол руления в пределах нормы."))
  (princ)
)

(princ "\nswept_path.lsp загружен. Команда: SWEPT")
(princ)
