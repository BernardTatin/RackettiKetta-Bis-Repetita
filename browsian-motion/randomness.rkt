#lang racket

(require math/statistics)

#|
    from "Simulations numériques du mouvement brownien confiné"
    by Elodie Millan
|#
(require racket/math)

(define pythagore-2
  (lambda(x y)
    (+ (* x x) (* y y))))

(define pythagore
  (lambda(x y)
    (sqrt (pythagore-2 x y))))

;; values in [-4.0, 4.0]
(define box-muller-rand
  (lambda()
    (let* ((u (random))
           (v (random))
           (X (* (sqrt (* -2 (log u))) (cos (* 2 pi v))))
           (Y (* (sqrt (* -2 (log u))) (sin (* 2 pi v)))))
      (values X Y))))

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

(define vector-of-random
  (lambda(rand-gen N)
    (for*/vector ([k N])
      (let-values (((X Y) (rand-gen)))
        X))))

(define-syntax fmt-n
    (syntax-rules ()
        ((_ x prec mwidth)
            (~a (~r x #:precision '(= prec)) #:width mwidth #:align 'right))))


(define stats-of-rand
    (lambda(name rand-f N)
        (let* ((samples (vector-of-random rand-f N))
                (median (median < samples))
                (avg (mean samples))
                (dev (stddev samples))
                (min (quantile 0 < samples))
                (max (quantile 1 < samples)))
            (printf "~a: [~a, ~a] med, avg: ~a, ~a dev~a~%"
                (~a name        #:width 20)
                ;; (~a (~r (- pi) #:precision 2) #:min-width 10 #:align 'right)
                (fmt-n min         2 6)
                (fmt-n max         2 6)
                (fmt-n median      2 6)
                (fmt-n avg         2 6)
                (fmt-n dev         2 6)))))




(stats-of-rand "multi-rand"         multi-rand      500)
(stats-of-rand "box-muller-rand"    box-muller-rand 500)
(stats-of-rand "Marsaglia-rand"     Marsaglia-rand  500)

