#lang racket

(require "../libs/rolling-cpt.rkt")

(display "======================================================================\n")
(display "rolling-cpt%\n")
(define x (new rolling-cpt%  (min0 1) [max0 6]))

(printf "i = ~a x = ~a~%" 0 (send x get))
(for ((i (in-range 1 15)))
    (printf "i = ~a x = ~a~%" i (send x next)))

(display "======================================================================\n")
(display "rolling-cpt-2d%\n")
(define xy (new rolling-cpt-2d% (xmax0 4) (ymax0 3)))
(let-values (((x y) (send xy get)))
  (printf "i = ~a x = ~a y = ~a~%" 0 x y))
(for ((i (in-range 1 15)))
  (let-values (((x y) (send xy next)))
    (printf "i = ~a x = ~a y = ~a~%" i x y)))

(display "======================================================================\n")
(display "simple-counter%\n")
(define cpt (new simple-counter% [cpt 1]))
(printf "i = ~a cpt = ~a~%" 0 (send cpt get))
(for ((i (in-range 1 15)))
    (printf "i = ~a cpt = ~a~%" i (send cpt next)))

