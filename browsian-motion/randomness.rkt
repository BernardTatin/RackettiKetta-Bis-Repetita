#lang racket

(require math/statistics)
(require plot)
(require plot/utils)

#|
    from "Simulations numériques du mouvement brownien confiné"
    by Elodie Millan
|#
(require racket/math)

(define-syntax fmt-n
    (syntax-rules ()
        ((_ x prec mwidth)
            (~a (~r x #:precision '(= prec)) #:width mwidth #:align 'right))))


(define pythagore-2
  (lambda(x y)
    (+ (* x x) (* y y))))

(define pythagore
  (lambda(x y)
    (sqrt (pythagore-2 x y))))

(define proj-X
  (lambda (X Y)
    X))

(define proj-Y
  (lambda (X Y)
    Y))

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
  (lambda(rand-gen N [k proj-X])
    (for*/vector ([idx N])
      (let-values (((X Y) (rand-gen)))
        (k X Y)))))

(define get-10th
    (lambda(samples [delta 0.1])
        (let ((N (inexact->exact (floor (/ 1.0 delta)))))
            (for*/vector ([k (+ 1 N)])
                (quantile (min (* k delta) 1.0) < samples)))))

(define print-10th
  (lambda(samples [delta 0.1] [q_ '()])
    (let ((q (if (null? q_) (get-10th samples delta) q_)))
      (printf "quantiles delta ~a ~%" (fmt-n delta 3 6))
      (letrec ((iloop
                (lambda(p k)
                  (when (<= p 1.0)
                    (printf "-> ~a : ~a~%"
                            (fmt-n p 3 6)
                            (fmt-n (vector-ref q k) 2 8))
                    (iloop (+ p delta) (+ k 1))))))
            (iloop 0 0)
            q))))

(define count-q
  (lambda(samples q)
    (foldl (lambda(v acc)
             (if (< v q)
                 (+ acc 1)
                 acc))
             0 samples)))

(define plot-q
    (lambda(samples q [delta 0.1])
        (letrec ((iloop
            (lambda(p k acc)
                (cond
                    [(> p 1) (reverse acc)]
                    [else (iloop (+ p delta) (+ k 1)
                            (cons (vector (count-q samples (vector-ref q k)) (min p 1.0)) acc))]))))
                            ; (cons (vector (vector-ref q k) (min p 1.0)) acc))]))))
            (plot (discrete-histogram (iloop 0 0 '()) )))))


    ;; (plot (discrete-histogram (list #(a 1) #(b 2) #(b 3) ...
(define stats-of-rand
  (lambda(name rand-f N)
    (let* ((samples (vector-of-random rand-f N proj-X))
           (delta 0.1)
           (median (median < samples))
           (avg (mean samples))
           (dev (stddev samples))
           (min (quantile 0 < samples))
           (max (quantile 1 < samples))
           )
      (printf "~a: [~a, ~a] med, avg: ~a, ~a dev~a~%"
              (~a name        #:width 20)
              (fmt-n min         2 6)
              (fmt-n max         2 6)
              (fmt-n median      2 6)
              (fmt-n avg         2 6)
              (fmt-n dev         2 6))
      (let ((q (print-10th samples delta)))
        (plot-new-window? #t)
        (plot-title name)
        (plot-q (vector->list samples) q delta)))))
(define safe-inv
  (lambda(x bad)
    (if (= 0 x)
      bad
      (/ 1.0 x))))

(define get-delta-q
  (lambda(q [delta 0.1])
    (let ((len (vector-length q)))
      (letrec ((iloop
                (lambda (k acc)
                  ;; k must be > 0
                  (if (= k (- len 1))
                      (reverse acc)
                      (iloop (+ k 1)
                        (let ((d (- (vector-ref q k) (vector-ref q (- k 1)))))
                             (cons (vector (* delta k) (safe-inv d 1)) acc)))))))
        (iloop 1 '())))))

(define stats-of-rand-k
  (lambda(name rand-f N [k proj-X])
    (let* ((samples (vector-of-random rand-f N k))
           (delta 0.02)
           (q (get-10th samples delta))
           (dq (get-delta-q q delta))
           )
      ; (print-10th samples delta q)
      ; (printf "---\n- ~a\n" dq)
      (plot-new-window? #t)
      (plot-title name)
      (plot (lines dq
                      #:marker 'fullcircle1
                      #:color "blue"
                      #:width 2)
        #:x-label "p(X)"
        #:y-min 0  #:y-label "1/dX")
      )))

(let ((N 55000))
  (stats-of-rand-k "multi-rand"         multi-rand      N proj-X)
  (stats-of-rand-k "box-muller-rand"    box-muller-rand N proj-X)
  (stats-of-rand-k "Marsaglia-rand"     Marsaglia-rand  N proj-X)
)
