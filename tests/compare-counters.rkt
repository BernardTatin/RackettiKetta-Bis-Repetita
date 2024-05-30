#lang racket

(require "../libs/rolling-cpt.rkt")
(require "../libs/cl-counters.rkt")

(define nloops 25000000)

(define chrono
  (lambda(name f)
    (let ((t0 (current-milliseconds)))
      (f nloops)
      (let ((dt (- (current-milliseconds) t0)))
        (printf "~a: ~ams\n" name dt)))))

(display "======================================================================\n")

(define x (cl-count  0  1))
(define cl-loop
  (lambda(n)
    (for ((i (in-range 0 n)))
      (x))
    (x)))

(define cpt (new simple-counter% [cpt 1]))
(define r-loop
    (lambda(n)
    (for ((i (in-range 0 n)))
      (send cpt next))
    (send cpt next)))

(chrono "cl-count" cl-loop)
(chrono "roll-count" r-loop)