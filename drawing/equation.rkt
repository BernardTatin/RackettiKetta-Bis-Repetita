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
(define hsb-max  255)

(define xl -2.0)
(define xh  2.0)
(define yl -2.0)
(define yh  2.0)

(define scf-x (get-scale-factor (0 def-width  xl xh)))
(define scf-y (get-scale-factor (0 def-height yl yh)))

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
            ; (scale-value (yg 0 height yl yh)))))

(define-syntax scale-x
    (syntax-rules ()
        ((_ (xg))
            (scale-value (xg 0 xl scf-x)))))

(define-syntax scale-color
    (syntax-rules ()
        ((_ (val))
            (scale-value (val val-min val-max hsb-min hsb-max)))))

(define compute-color
  (lambda (val)
    (if val-ok?
        (scale-color (val))
        (cond
          [(or (not val-max) (= val-max val-min)) hsb-min]
        ;   [(< val val-min) hsb-min]
        ;   [(> val val-max) hsb-max]
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
  (let* ((x2 (* x x))
         (y2 (* y y))
         (s2 (+ x2 y2))
         (a 0.5)
         (val0 (+ (* x2 x2) (- x2) y2))
         (val1 (- (* s2 s2) (* a (- x2 y2))))
         (val2 (+ (* x y) (- x2 y2)))
         (val val2))
    ; (printf "~a ~a -> ~a~%"
    ;     (~r   x #:precision '(= 2) #:min-width 8)
    ;     (~r   y #:precision '(= 2) #:min-width 8)
    ;     (~r val #:precision '(= 2) #:min-width 8)
    ; )
    (if val-ok?
        val
        (cond
          ((not val-max) (:= val-max val) (:= val-min val) val)
          ((< val val-min) (:= val-min val) val)
          ((> val val-max) (:= val-max val) val)
          (else val)))))


(define (draw)
  (load-pixels)
  (for ([i width])
    (for ([j height])
      (let* ([val   (equation (scale-x (i)) (scale-y (j)))]
             [col (compute-color val)])
        (set-pixel i j (color col col col)))))
  (when (not val-ok?)
    (disp-extrems)
    (display-all (list "max -> " (scale-color (val-max))
                       " or " (- hsb-max hsb-min)))
    (:= val-ok? #t))
  (update-pixels))


