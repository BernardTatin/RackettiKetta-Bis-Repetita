#lang racket

(require sketching)

(provide display-all
         number->int
         with-pixels
         get-scale-factor
         def-scale-factor
         scale-value)

(define (display-all . lst)
    (for-each (lambda(e)
        (display e))
            lst))

(define-syntax number->int
  (syntax-rules ()
    ((_ x) (inexact->exact (floor x)))))

#|
    Some macros to help drawing
|#

(define-syntax with-pixels
  (syntax-rules ()
    ((_ () body ...)
     (begin
       (load-pixels)
       body ...
       (update-pixels)))))


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
