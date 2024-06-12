#lang sketching

(require "../libs/draw-utils.rkt")
#|
Sketching example: Graphing 2D Equation

This implementation is a close translation from the "Graphing 2D Equation" example from https://processing.org/examples/graphing2dequation.html.

Currently, pixel operations are very slow.
|#

(define def-width  800)
(define def-height 600)

(define val-min   #f)
(define val-max   #f)
(define val-ok? #f)

(define hsb-min    0)
(define hsb-max  512)

(define xl -1.0)
(define xh  1.0)
(define yl -1.0)
(define yh  1.0)

(define scf-x (get-scale-factor (0 def-width  xl xh)))
(define scf-y (get-scale-factor (0 def-height yl yh)))
(define scf-h #f)

(define (on-resize width height)
    (:= val-ok? #f)
    (:= scf-x (get-scale-factor (0 width  xl xh)))
    (:= scf-y (get-scale-factor (0 height yl yh))))

(define (disp-extrems)
  (for-each (lambda(e)
              (display e))
              (list "val-min: " val-min "  val-max: " val-max " hsb-min " hsb-min " hsb-max " hsb-max))
  (display "\n"))

(define display-all
  (lambda(lst)
    (for-each (lambda(e)
                (display e))
              lst)
    (display "\n")))

(define-syntax scale-y
    (syntax-rules ()
        ((_ (yg))
            (scale-value (yg 0 yl scf-y)))))

(define-syntax scale-x
    (syntax-rules ()
        ((_ (xg))
            (scale-value (xg 0 xl scf-x)))))

(define-syntax scale-color
  (syntax-rules ()
    ((_ (val))
     (if scf-h
         (scale-value (val val-min hsb-min scf-h))
         (scale-value (val val-min val-max hsb-min hsb-max))))))

(define compute-color
  (lambda (val)
    (if val-ok?
        (scale-color (val))
        (cond
          [(or (not val-max) (= val-max val-min)) hsb-min]
          [else
           (scale-color (val))]))))

(define (setup)
  (size def-width def-height)
  (color-mode 'hsb hsb-max)
  (frame-rate 20))

#|
    lemniscate
|#
(define (equation x y)
  (let (
        ;  (x2 (* x x))
        ;  (y2 (* y y))
        ;  (s2 (+ x2 y2))
        ;  (a 0.5)
        ;  (val0 (- (* x (+ x2 x2)) (* a (- (* 3 x2) y2))))
        ;  (val1 (- (* s2 s2) (* a (- x2 y2))))
        (val (* x y)))
    (if val-ok?
        val
        (cond
          ((not val-max) (:= val-max val) (:= val-min val) val)
          ((< val val-min) (:= val-min val) val)
          ((> val val-max) (:= val-max val) val)
          (else val)))))


;; 420/425 ms shb-max =  256
;; 775/790 ms shb-max = 2560
(define (draw0)
  (with-pixels ()
      (let ((now (current-milliseconds))
            (dt 0))
        (when (not val-ok?)
          (for ([i width])
            (for ([j height])
              (let* ((col0 i)
                     (col (min col0 hsb-max)))
                (set-pixel i j (color col col col)))))
          (:= val-ok? #t))
        (:= dt (- (current-milliseconds) now))
        (display-all (list "temps " dt "ms")))))

;; 500/510 ms -> hsb-max  256
;; 520/530 ms -> hsb-max 2560
(define (draw)
  (let ((now (current-milliseconds))
        (dt 0))
    (with-pixels ()
      (when (not val-ok?)
        (let* ((dx (/ (- xh xl) width))
               (dy (/ (- yh yl) height))
               (x xl)
               (y yl))
          (for ([i width])
            (:= y yl)
            (for ([j height])
              (let ([col (- hsb-max (compute-color (equation x y)))])
                (set-pixel i j (color col col col))
                (:= y (+ y dy))))
            (:= x (+ x dx))
            ))))

    (:= dt (- (current-milliseconds) now))
    (display-all (list "temps " dt "ms")))
  (when (not val-ok?)
    (disp-extrems)
    (display-all (list "max -> " (scale-color (val-max))
                       " or " (- hsb-max hsb-min)))
    (:= scf-h (get-scale-factor (val-min val-max hsb-min hsb-max)))
    (:= val-ok? #t)))


