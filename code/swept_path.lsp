;;; swept_path.lsp -- simplified swept path analysis for AutoCAD (AutoLISP)
;;;
;;; Command: SWEPT   (all prompts are in transliterated Russian, ASCII only)
;;;
;;; Simulates a vehicle driving along a path and draws:
;;;   - body outlines at a given interval          (layer SWEPT-BODY)
;;;   - traces of the four body corners            (layer SWEPT-ENV)
;;;   - traces of both axle ends = wheels          (layer SWEPT-WHEEL)
;;;   - traces of axle centres: front (= the path) and rear (layer SWEPT-AXLE)
;;;   - for Semi: traces of points along the trailer sides (layer SWEPT-SIDE)
;;;
;;; Envelope boundaries (see pathsweeper.com/en/guides/swept-path-analysis):
;;;   outer edge = trace of the outer FRONT CORNER of the body;
;;;   inner edge = trace of the inner REAR WHEEL (not the corner: the corner
;;;                is farther from the turn centre and gives a too narrow band);
;;;   outer rear corner = tail swing at the start of a turn.
;;;
;;; Steering limit is derived from the OUTER turning radius (at the body or at
;;; the outer front wheel, see below; for cars the body radius is half of the
;;; wall-to-wall turning circle diameter).
;;; At the end the required steering angle is reported; if it exceeds the
;;; limit, the vehicle cannot follow this path.
;;;
;;; Model: kinematic, bicycle. The selected curve is the path of the FRONT
;;; axle centre. The rear axle centre follows it at a distance equal to the
;;; wheelbase (no lateral slip of the rear wheels).
;;;
;;; Semi (articulated truck): tractor + semi-trailer. The kingpin lies on the
;;; tractor axis, distance fw ahead of its rear axle; the trailer axle (bogie
;;; centre) follows the kingpin at distance ltk -- same "follower chases the
;;; leader" scheme as the rear axle of the tractor. Four corners are not
;;; enough for an articulated vehicle (the inner edge may be set by a point on
;;; the trailer side), hence the extra side traces.
;;;
;;; Turning radius can be given at the BODY (outer front corner; DIN 70020
;;; "wall-to-wall") or at the outer front WHEEL (track circle, what Russian
;;; data sheets usually list). A wheel radius must NOT be used as a body
;;; radius: the body corner is farther out and the envelope would be too narrow.
;;;
;;; Presets: Car = example from the article (VW Golf / BMW 3 class).
;;;   Fire = fire engine AC 9.0 on KAMAZ-65111 (6x6), data from the user's list
;;;   (not checked against a data sheet): wheelbase 4.1 m (front axle to the
;;;   middle of the rear bogie), front overhang 1.42, width 2.5, tracks
;;;   2.05 / 1.9, turning radius 11.3 m at the outer WHEEL. The list gives the
;;;   rear overhang as 2.1-2.3 m but the overall length as 9.1-9.3 m; these do
;;;   not add up (1.42 + 4.1 + 2.2 = 7.72 m). The preset takes the overhang
;;;   that matches the length (3.7 m, measured from the bogie middle) because a
;;;   too short tail UNDERestimates tail swing. Enter 2.2 to use the list value.
;;;   Truck, Bus and the Semi tractor are estimates -- enter your own values.
;;;   Wheel traces are drawn at track/2 + 0.15 m (half a tyre), see sp:tyre.
;;;
;;; Known limits (deliberate):
;;;   - one semi-trailer only; no drawbar trailers, no B-trains;
;;;   - forward motion only, in the direction the curve was drawn;
;;;   - a sharp polyline vertex gives a jump of the steering angle -- draw a
;;;     smooth path (arcs, spline, filleted polyline);
;;;   - accuracy depends on the step: at 0.25 m the error is well below 1 cm
;;;     for radii above ~5 m; for tighter radii use a smaller step.

(vl-load-com)

(setq sp:tyre 0.15)                     ; half tyre width, m (assumption)

;; --- helpers ----------------------------------------------------------------

(defun sp:ask (msg def / x)
  (initget 6)                           ; > 0 only
  (setq x (getreal (strcat "\n" msg " <" (rtos def 2 2) ">: ")))
  (if x x def)
)

(defun sp:ask0 (msg def / x)            ; zero allowed
  (initget 4)                           ; >= 0 only
  (setq x (getreal (strcat "\n" msg " <" (rtos def 2 2) ">: ")))
  (if x x def)
)

;; normalise angle to (-pi; pi]
(defun sp:angdiff (a b / d)
  (setq d (- a b))
  (while (> d pi) (setq d (- d (* 2 pi))))
  (while (<= d (- pi)) (setq d (+ d (* 2 pi))))
  d
)

;; point from origin o with heading ang: dx forward, dy to the left
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

;; is the object usable as a path (a curve with parameters)?
(defun sp:curve-p (en / r)
  (setq r (vl-catch-all-apply 'vlax-curve-getEndParam (list en)))
  (and (not (vl-catch-all-error-p r)) (numberp r))
)

;; --- main command -----------------------------------------------------------

(defun c:SWEPT (/ sc kw pr wb ovf ovr wd rout rrear maxst ds ivl sel en total
                  n i dist p0 tg fp f r h v len vel delta maxd nextd
                  fl fr rl rr trfl trfr trrl trrr trf trr nbody rmin
                  twl twr tfl2 tfr2
                  semi fw ltk tfo tro tw amax tp ht kp art maxa
                  ufl ufr url urr utfl utfr utrl utrr uwl uwr utp
                  stns usl usr cl cr tr1
                  ftk rtk rty fwo rwo rbody)

  ;; --- input
  (setq sc (sp:ask "Edinits chertezha v 1 m (m=1, mm=1000)" 1.0))

  (initget "Car Truck Bus Semi Fire")
  (setq kw (getkword "\nTip TS [Car/Truck/Bus/Semi(avtofura)/Fire(pozhmash)] <Truck>: "))
  (if (null kw) (setq kw "Truck"))
  ;; wheelbase, front overhang, rear overhang, width, turning radius,
  ;; front track, rear track (m), radius type (Kuzov = body, Koleso = wheel)
  ;; Rear overhang is measured from the rear axle reference point (for a
  ;; bogie: its middle, the same point the wheelbase is measured to).
  (setq pr (cond ((= kw "Car")   '(2.7 0.9 1.1 1.85 5.6 1.55 1.55 "Kuzov"))
                 ((= kw "Bus")   '(6.0 2.7 3.3 2.55 12.1 2.25 2.25 "Kuzov"))
                 ((= kw "Semi")  '(3.8 1.4 0.8 2.55 7.6 2.25 2.25 "Kuzov"))
                 ((= kw "Fire")  '(4.1 1.42 3.7 2.5 11.3 2.05 1.9 "Koleso"))
                 (t              '(4.5 1.5 2.5 2.5 9.8 2.2 2.2 "Kuzov"))))
  (setq wb    (sp:ask "Baza (do serediny zadnei telezhki), m" (nth 0 pr))
        ovf   (sp:ask "Perednii sves, m"                     (nth 1 pr))
        ovr   (sp:ask "Zadnii sves (ot serediny telezhki), m" (nth 2 pr))
        wd    (sp:ask "Shirina kuzova, m"                    (nth 3 pr))
        ftk   (sp:ask "Koleya perednikh koles, m"            (nth 5 pr))
        rtk   (sp:ask "Koleya zadnikh koles, m"              (nth 6 pr)))
  (initget "Kuzov Koleso")
  (setq rty (getkword (strcat "\nRadius razvorota izmeren po [Kuzov/Koleso] <"
                              (nth 7 pr) ">: ")))
  (if (null rty) (setq rty (nth 7 pr)))
  (setq rout  (sp:ask (strcat "Radius razvorota (" rty "), m") (nth 4 pr))
        ds    (sp:ask "Shag modelirovaniya, m"               0.25)
        ivl   (sp:ask "Interval konturov kuzova, m"          3.0))

  (setq semi (= kw "Semi"))
  (if semi
    ;; semi-trailer: 7.7 + 4.3 = 12.0 m from kingpin to the rear edge (EU
    ;; limit), 1.6 m ahead of the kingpin -- 13.6 m in total; check the data sheet
    (setq fw   (sp:ask0 "Shkvoren vperedi zadnei osi tyagacha, m"   0.0)
          ltk  (sp:ask  "Shkvoren -- os (telezhka) polupritsepa, m" 7.7)
          tfo  (sp:ask  "Polupritsep: sves vpered ot shkvorenya, m" 1.6)
          tro  (sp:ask  "Polupritsep: sves nazad za osyu, m"        4.3)
          tw   (sp:ask  "Shirina polupritsepa, m"                    2.55)
          amax (sp:ask  "Maks. ugol v sedelnom ustroistve, grad"     90.0)))

  ;; rear axle radius at full lock (Rrear = distance from the turn centre to
  ;; the rear axle centre):
  ;;   body:  R_out^2 = (Rrear + w/2)^2   + (wb+fo)^2
  ;;   wheel: R_out^2 = (Rrear + ftk/2)^2 + wb^2
  (if (= rty "Koleso")
    (progn
      (if (<= rout wb)
        (progn (princ "\nRadius po kolesu ne bolshe bazy -- tak ne byvaet.") (exit)))
      (setq rrear (- (sqrt (- (* rout rout) (* wb wb))) (/ ftk 2.0))))
    (progn
      (if (<= rout (+ wb ovf))
        (progn (princ "\nVneshnii radius ne bolshe rasstoyaniya ot zadnei osi do bampera -- tak ne byvaet.")
               (exit)))
      (setq rrear (- (sqrt (- (* rout rout) (* (+ wb ovf) (+ wb ovf)))) (/ wd 2.0)))))
  (if (<= rrear 0.0)
    (progn (princ "\nRadius slishkom mal dlya etikh razmerov.") (exit)))
  (setq maxst (* (atan wb rrear) (/ 180.0 pi)))     ; steering angle, deg
  ;; equivalent radius of the outer front body corner (for the report)
  (setq rbody (sqrt (+ (* (+ rrear (/ wd 2.0)) (+ rrear (/ wd 2.0)))
                       (* (+ wb ovf) (+ wb ovf)))))

  ;; to drawing units
  (setq fwo (* (+ (/ ftk 2.0) sp:tyre) sc)          ; wheel offsets from axis
        rwo (* (+ (/ rtk 2.0) sp:tyre) sc))
  (setq wb (* wb sc) ovf (* ovf sc) ovr (* ovr sc) wd (* wd sc)
        ds (* ds sc) ivl (* ivl sc))
  (if semi
    (setq fw (* fw sc) ltk (* ltk sc) tfo (* tfo sc) tro (* tro sc) tw (* tw sc)))

  ;; --- path selection
  (setq en nil)
  (while (null en)
    (setq sel (entsel "\nTraektoriya serediny perednei osi (polilinia/duga/splain): "))
    (cond ((null sel) (princ "\nOtmena.") (exit))
          ((sp:curve-p (car sel)) (setq en (car sel)))
          (t (princ "\nEto ne krivaya, vyberite drugoi obekt."))))

  (setq total (vlax-curve-getDistAtParam en (vlax-curve-getEndParam en)))
  (if (< total (* 2 ds))
    (progn (princ "\nTraektoriya slishkom korotkaya dlya vybrannogo shaga.") (exit)))

  (sp:layer "SWEPT-BODY" 8)
  (sp:layer "SWEPT-ENV"  1)
  (sp:layer "SWEPT-AXLE" 5)
  (sp:layer "SWEPT-WHEEL" 3)
  (sp:layer "SWEPT-SIDE" 9)

  (command "_.undo" "_begin")

  ;; --- initial state: vehicle stands along the tangent at the curve start
  (setq p0 (vlax-curve-getPointAtDist en 0.0)
        tg (vlax-curve-getFirstDeriv en (vlax-curve-getStartParam en))
        h  (atan (cadr tg) (car tg))
        r  (sp:pt p0 h (- wb) 0.0)       ; rear axle one wheelbase behind the front
        fp p0
        maxd 0.0
        nextd 0.0
        nbody 0
        maxa 0.0
        n  (fix (/ total ds)))
  (if semi
    (setq kp   (sp:pt r h fw 0.0)
          tp   (sp:pt kp h (- ltk) 0.0)  ; trailer starts straight
          ht   h
          stns (list ltk (* 0.5 ltk) (* -0.5 tro))
          usl  (mapcar '(lambda (x) nil) stns)
          usr  (mapcar '(lambda (x) nil) stns)))

  ;; list of distances: 0, ds, 2ds, ... and the exact end
  (setq i 0)
  (repeat (1+ n)
    (setq dist (cons (* i ds) dist) i (1+ i)))
  (if (< (car dist) (- total 1e-9)) (setq dist (cons total dist)))
  (setq dist (reverse dist))

  ;; --- step-by-step simulation
  (foreach d dist
    (setq f (vlax-curve-getPointAtDist en d))
    (if (> d 0.0)
      (progn
        ;; rear axle looks at the front one: |F-R| = wb, R moves along the heading
        (setq v   (list (- (car f) (car r)) (- (cadr f) (cadr r)))
              len (distance '(0.0 0.0) v)
              h   (atan (cadr v) (car v))
              r   (sp:pt f h (- wb) 0.0))
        ;; steering angle = front axle velocity direction relative to heading
        (if (> (distance f fp) 1e-9)
          (progn
            (setq vel   (angle fp f)
                  delta (abs (sp:angdiff vel h)))
            (if (> delta maxd) (setq maxd delta))))
      )
    )
    (setq fp f)

    ;; body corners (rear axle = r, heading = h)
    (setq fl (sp:pt r h (+ wb ovf) (/ wd 2.0))
          fr (sp:pt r h (+ wb ovf) (- (/ wd 2.0)))
          rl (sp:pt r h (- ovr)     (/ wd 2.0))
          rr (sp:pt r h (- ovr)     (- (/ wd 2.0))))
    (setq trfl (cons fl trfl) trfr (cons fr trfr)
          trrl (cons rl trrl) trrr (cons rr trrr)
          trf  (cons f trf)   trr  (cons r trr)
          ;; axle ends = outer tyre edges (track/2 + half tyre)
          twl  (cons (sp:pt r h 0.0 rwo) twl)
          twr  (cons (sp:pt r h 0.0 (- rwo)) twr)
          tfl2 (cons (sp:pt r h wb fwo) tfl2)
          tfr2 (cons (sp:pt r h wb (- fwo)) tfr2))

    ;; semi-trailer: axle chases the kingpin, heading = axle -> kingpin
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
        ;; points along the sides (the inner edge may be neither corner nor wheel)
        (setq cl  (mapcar '(lambda (x) (sp:pt tp ht x (/ tw 2.0))) stns)
              cr  (mapcar '(lambda (x) (sp:pt tp ht x (- (/ tw 2.0)))) stns)
              usl (mapcar '(lambda (a q) (cons q a)) usl cl)
              usr (mapcar '(lambda (a q) (cons q a)) usr cr))))

    ;; body outline every interval and at the last point
    (if (or (>= d nextd) (= d total))
      (progn
        (sp:pline (list fl fr rr rl) T "SWEPT-BODY")
        (if semi (sp:pline (list ufl ufr urr url) T "SWEPT-BODY"))
        (setq nbody (1+ nbody))
        (while (<= nextd d) (setq nextd (+ nextd ivl)))))
  )

  ;; --- traces
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

  ;; --- report
  (princ (strcat "\nShagov: " (itoa (length dist))
                 ", konturov kuzova: " (itoa nbody)
                 ", dlina puti: " (rtos (/ total sc) 2 2) " m"))
  (princ (strcat "\nDlina TS po modeli: "
                 (rtos (/ (+ wb ovf ovr) sc) 2 2) " m; raschetnyi radius po kuzovu: "
                 (rtos rbody 2 2) " m"))
  (princ (strcat "\nMaks. ugol rulenia na traektorii: "
                 (rtos (* maxd (/ 180.0 pi)) 2 1) " grad (predel "
                 (rtos maxst 2 1) ")"))
  (if (> maxd 1e-6)
    (progn
      (setq rmin (/ (/ wb (sin (min maxd (/ pi 2.0)))) sc))
      (princ (strcat "\nMin. radius po osi perednikh koles na traektorii: "
                     (rtos rmin 2 2) " m"))))
  (if semi
    (progn
      (princ (strcat "\nMaks. ugol v sedelnom ustroistve: "
                     (rtos (* maxa (/ 180.0 pi)) 2 1) " grad (predel "
                     (rtos amax 2 1) ")"))
      (if (> (* maxa (/ 180.0 pi)) amax)
        (princ "\n*** VNIMANIE: ugol skladyvaniya previshaet predel -- tyagach upretsya v polupritsep. ***"))))
  (if (> (* maxd (/ 180.0 pi)) maxst)
    (princ "\n*** VNIMANIE: trebuemyi ugol rulenia previshaet predel -- TS tak ne proedet, smyagchite traektoriyu. ***")
    (princ "\nUgol rulenia v predelakh normy."))
  (princ)
)

(princ "\nswept_path.lsp zagruzhen. Komanda: SWEPT")
(princ)
