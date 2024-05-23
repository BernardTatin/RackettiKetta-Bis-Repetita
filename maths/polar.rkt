#lang racket

(require plot)
(plot-new-window? #t)
(plot-title "")

(define from (- (* 5 pi)))
(define to   (* 5 pi))

(plot (list
        (axes)
        (parametric (λ (t) (vector (cos t) (sin (* 2 t)))) from to #:label "parametric")
        (polar (λ (t) (- (sin t) (cos (* 2 t)))) from to #:label "polar")))