#lang racket

(require "../libs/timing.rkt")

(let ((t (with-timing ()
           (sleep 0.1))))
  (printf "sleep 0.1 -> ~a~%" t))

(let ((t (with-stats-timing (50)
           (sleep 0.01))))
;   (printf "sleep 0.1 -> ~a~%" t)
  (displayln (stats->string t 6)))