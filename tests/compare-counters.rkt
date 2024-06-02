#lang racket

(require "../libs/rolling-cpt.rkt")
(require "../libs/cl-counters.rkt")
(require "../libs/timing.rkt")

(define nloops 25000000)
(define chrono-loops 15)

(define-syntax do-test
  (syntax-rules ()
    ((_ (name) body ...)
      (let ((stats (with-stats-timing (chrono-loops)
            (for ((i (in-range 0 nloops)))
              body ...))))
            (printf "~a : ~a\n" (~a name #:min-width 11)
              (stats->string stats))))))

(let ((cpt (cl-count 0 1)))
  (do-test ("cl-count") (cpt)))

(let ((cpt (cl-count+ 0 1)))
  (do-test ("cl-count+") (cpt 'next)))

(let ((cpt (cl-rcount-2d 0 800 0 640)))
  (do-test ("cl-count+") (cpt)))


; (make-loop ("cl-count-c-"
;             c (cl-count-c- 0 1) (c))
;            (c))

; (make-loop ("cl-count-c++"
;             c(cl-count-c+ 0 1) (c 'next))
;            (c 'get))

;; presque 30 fois plus lent que les précédents
#|
(make-loop ("simple-c..%"
            c (new simple-counter% [cpt 1]) (send c next))
           (send c next))
|#

; (for ((i (in-range 0 8)))
;   (let ((now (current-milliseconds)))
;     (with-noset-count ((cl-count-noset 0 1)
;                        (lambda (current) (< current nloops)))
;       (lambda(current) current))
;     (printf "loop ~a -> ~ams\n" i (- (current-milliseconds) now))))

