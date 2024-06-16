#lang racket

(require "../libs/some-maths.rkt")

(provide box-muller-rand
         Marsaglia-rand
         rand-bm
         multi-rand
         rand-1
         rand-bm-ivl
         )

(define-syntax rand-1
    (syntax-rules ()
        ((_ rand-function)
            (let-values (((x y) (rand-function)))
                x))))

;; values in [-4.0, 4.0]
(define box-muller-rand
  (lambda()
    (let* ((u (random))
           (v (random))
           (X (* (sqrt (* -2 (log u))) (cos (* 2 pi v))))
           (Y (* (sqrt (* -2 (log u))) (sin (* 2 pi v)))))
      (values X Y))))

(define rand-bm
    (lambda()
        (let-values (((x y) (box-muller-rand)))
            ;; from [-4, 4] to [0, 1]
            ;; 0 + (x - -4) (1 - 0) / (4 - -4)
            ;; (x + 4) / 8
            (values (/ (+ x 4) 8) (/ (+ y 4) 8)))))

(define rand-bm-ivl-0
    (lambda([from 0.0] [to 1.0])
        (let ((r (rand-1 rand-bm)))
            (cond
                ((and (= 0.0 from) (= 1.0 to)) r)
                (else
                    ;; from [0.0, 1.0]to [from, to]
                    ;; from + (to - from) (r - 0) / (1 - 0)
                    ;; from + r(to -from)
                    (+ from (* r (- to from))))))))

(define rand-bm-ivl
  (case-lambda
    [()         (rand-1 rand-bm)]
    [(to)       (* to (rand-1 rand-bm))]
    [(from to)  (+ from (* (rand-1 rand-bm) (- to from)))]))

(define Marsaglia-rand
  (lambda()
    (let ((compute-UV
           (lambda()
             (- (* 2.0 (random)) 1))))
      (letrec ((iloop
                (lambda (S U V)
                  (cond
                    [(< S 1.0)
                     (let* ((X (* U (sqrt (* -2 (/ (log S) S)))))
                            (Y (* V (sqrt (* -2 (/ (log S) S))))))
                       (values X Y))]
                    [else
                     (iloop
                      (pythagore-2 U V)
                      (compute-UV)
                      (compute-UV))]))))
        (iloop 2.0 (compute-UV) (compute-UV))))))

(define multi-rand
  (lambda()
    (values (random) (random))))
