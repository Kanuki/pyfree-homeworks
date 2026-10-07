;;; swept_path.lsp -- упрощённый аналог Swept Path Analysis для AutoCAD (AutoLISP)
;;;
;;; Команда: SWEPT
;;;   Моделирует проезд двухосного ТС по траектории и строит:
;;;     - контуры кузова через заданный интервал  (слой SWEPT-BODY)
;;;     - следы четырёх углов кузова = огибающая  (слой SWEPT-ENV)
;;;     - следы осей: передняя (по траектории) и задняя (слой SWEPT-AXLE)
;;;   В конце выводит максимальный угол руления и предупреждает, если он
;;;   превышает предел для выбранного ТС (т.е. ТС по такой траектории не проедет).
;;;
;;; Модель: кинематическая, велосипедная.
;;;   Выбранная кривая = траектория середины ПЕРЕДНЕЙ оси.
;;;   Середина задней оси движется "за" передней на расстоянии базы L
;;;   (нет бокового проскальзывания задних колёс).
;;;
;;; Ограничения (сознательные):
;;;   - только одиночное ТС без прицепа (сочленённые не поддерживаются);
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

(defun c:SWEPT (/ sc kw pr wb ovf ovr wd maxst ds ivl sel en total n i dist
                  p0 tg fp f r h v len vel delta maxd nextd
                  fl fr rl rr trfl trfr trrl trrr trf trr bodies nbody
                  rmin)

  ;; --- ввод параметров
  (setq sc (sp:ask "Единиц чертежа в 1 м (м=1, мм=1000)" 1.0))

  (initget "Car Truck Bus")
  (setq kw (getkword "\nТип ТС [Car/Truck/Bus] <Truck>: "))
  (if (null kw) (setq kw "Truck"))
  ;; база, передний свес, задний свес, ширина (м), макс. угол руления (град)
  (setq pr (cond ((= kw "Car")   '(2.7 0.9 1.0 1.8 35.0))
                 ((= kw "Bus")   '(6.0 2.7 3.3 2.55 40.0))
                 (t              '(4.5 1.5 2.5 2.5 35.0))))
  (setq wb    (sp:ask "База (между осями), м"      (nth 0 pr))
        ovf   (sp:ask "Передний свес, м"           (nth 1 pr))
        ovr   (sp:ask "Задний свес, м"             (nth 2 pr))
        wd    (sp:ask "Ширина кузова, м"           (nth 3 pr))
        maxst (sp:ask "Макс. угол руления, град"   (nth 4 pr))
        ds    (sp:ask "Шаг моделирования, м"       0.25)
        ivl   (sp:ask "Интервал контуров кузова, м" 3.0))

  ;; в единицы чертежа
  (setq wb (* wb sc) ovf (* ovf sc) ovr (* ovr sc) wd (* wd sc)
        ds (* ds sc) ivl (* ivl sc))

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
        n  (fix (/ total ds)))

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
          trf  (cons f trf)   trr  (cons r trr))

    ;; контур кузова через интервал и в последней точке
    (if (or (>= d nextd) (= d total))
      (progn
        (sp:pline (list fl fr rr rl) T "SWEPT-BODY")
        (setq nbody (1+ nbody))
        (while (<= nextd d) (setq nextd (+ nextd ivl)))))
  )

  ;; --- следы
  (sp:pline (reverse trfl) nil "SWEPT-ENV")
  (sp:pline (reverse trfr) nil "SWEPT-ENV")
  (sp:pline (reverse trrl) nil "SWEPT-ENV")
  (sp:pline (reverse trrr) nil "SWEPT-ENV")
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
  (if (> (* maxd (/ 180.0 pi)) maxst)
    (princ "\n*** ВНИМАНИЕ: требуемый угол руления превышает предел -- ТС так не проедет, смягчите траекторию. ***")
    (princ "\nУгол руления в пределах нормы."))
  (princ)
)

(princ "\nswept_path.lsp загружен. Команда: SWEPT")
(princ)
