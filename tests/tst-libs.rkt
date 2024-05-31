#lang racket

(require "../libs/rolling-cpt.rkt")
(require "../libs/cl-counters.rkt")

(display "======================================================================\n")
(display "rolling-cpt%\n")
(define x (new rolling-cpt%  (vmin 1) [vmax 6]))

(printf "i = ~a x = ~a~%" 0 (send x get))
(for ((i (in-range 1 15)))
    (printf "i = ~a x = ~a~%" i (send x next)))

(display "======================================================================\n")
(display "rolling-cpt-2d%\n")
(define xy (new rolling-cpt-2d% (xmax 4) (ymax 3)))
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

(display "======================================================================\n")
(displayln "cl-count")
(let ((x+ (cl-count  0  1))
      (x- (cl-count 14 -1)))
  (for ((i (in-range 0 15)))
    (printf "i: ~a, cpt: ~a - ~a~%" i (x+) (x-))))


(display "======================================================================\n")
(displayln "cl-count+")
(let ((x+ (cl-count+  0  1))
      (x- (cl-count+ 14 -1)))
  (printf "i: ~a, cpt: ~a - ~a~%" 0 (x+ 'get) (x- 'get))
  (for ((i (in-range 1 15)))
    (printf "i: ~a, cpt: ~a - ~a~%" i (x+ 'next) (x- 'next))))


(display "======================================================================\n")
(displayln "cl-rcount")
(let ((x+ (cl-rcount  0 4 1)))
  (for ((i (in-range 1 15)))
    (printf "i: ~a, cpt: ~a~%" i (x+))))


(display "======================================================================\n")
(displayln "cl-rcount-2d")
(let ((x+ (cl-rcount-2d  0 4 0 4 1)))
  (for ((i (in-range 1 25)))
    (let-values (((x y) (x+)))
      (printf "i: ~a, cpt: ~a, ~a~%" i x y))))

