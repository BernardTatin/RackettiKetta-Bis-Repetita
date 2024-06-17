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
            (values (* (+ x 4) 1/8) (* (+ y 4) 1/8)))))

;; return a value in [0, 1]
(define box-muller-rand-1
  (lambda()
    (let* ((u (log (random)))
           (v (* 2 pi (random)))
           (r (* (sqrt (* -2 u)) (cos v))))
          (* (+ r 4) 1/8))))

(define rand-bm-ivl
  (case-lambda
    [()         (box-muller-rand-1)]
    [(to)       (* to (box-muller-rand-1))]
    [(from to)  (+ from (* (box-muller-rand-1) (- to from)))]))

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
