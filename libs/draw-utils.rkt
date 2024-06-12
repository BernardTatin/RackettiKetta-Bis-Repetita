#lang racket

(provide get-scale-factor
         def-scale-factor
         scale-value)
#|
    Some macros to help drawing
|#

(define-syntax get-scale-factor
    (syntax-rules ()
        ((_ (vgl vgh vl vh))
            (/  (- vh vl) (- vgh vgl)))))

(define-syntax def-scale-factor
    (syntax-rules ()
        ((_ (name vgl vgh vl vh))
            (define name (get-scale-factor (vgl vgh vl vh))))))

(define-syntax scale-value
    (syntax-rules ()
        ((_ (vg vgl vgh vl vh))
            (+ vl (/ (* (- vh vl) (- vg vgl)) (- vgh vgl))))
        ((_ (vg vgl vgh vl vh scf))
            (+ vl (* scf (- vg vgl))))
        ((_ (vg vgl vl scale-factor))
            (+ vl (* scale-factor (- vg vgl))))
            ))

(define test-scale
    (lambda()
        (let* ((xgl   0)
               (xgh 640)
               (xl    0.0)
               (xh    1.0)
               (yl   -1.0)
               (yh    1.0)

               (xg0 320)
               (x0    0.5)
               (y0    0.0))
    (printf "xg0 (~a) -> ~a, expect ~a~%"
        xg0
        (scale-value (xg0 xgl xgh xl xh))
        0.5)
    (printf "x0  (~a) -> ~a, expect ~a~%"
        x0
        (scale-value (x0 xl xh xgl xgh))
        320)
    (printf "y0  (~a) -> ~a, expect ~a~%"
        y0
        (scale-value (y0  yl yh xgl xgh))
        320)
        )))
; (test-scale)


