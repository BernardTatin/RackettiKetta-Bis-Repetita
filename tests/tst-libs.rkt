#lang racket

(require "../libs/rolling-cpt.rkt")

(define x (new rolling-cpt%  [max0 6]))

(printf "i = ~a x = ~a~%" 0 (send x get))
(for ((i (in-range 0 15)))
    (printf "i = ~a x = ~a~%" i (send x next)))