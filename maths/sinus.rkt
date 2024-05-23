#lang racket

(require plot)
(plot-new-window? #t)
(plot-title "Des sinus et des cosinus")

(define from (- (* 2 pi)))
(define to   (* 2 pi))
(define f+
    (lambda(x)
        (+ (cos x) (sin x))))

(define f-
    (lambda(x)
        (- (cos x) (sin x))))

(plot (list
        (axes)
        (function f+  from to #:label "y = cos(x) + sin(x)")
        (function f-  from to #:label "y = cos(x) - sin(x)")
        (function cos from to #:label "y = cos(x)")
        (function sin from to #:label "y = sin(x)")))