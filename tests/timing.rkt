#lang racket

(require plot)
(require "../libs/timing.rkt")

(let ((t (with-timing ()
           (sleep 0.1))))
  (printf "sleep 0.01 -> ~a~%" t))

(let ((t (with-stats-timing (50)
           (sleep 0.01))))
  (let ((xs (range 0 (length t))))
    (plot-new-window? #t)
    (plot-title "sleep 10 ms")
    (parameterize ([plot-width    550]
                   [plot-height   550]
                   [plot-x-label  #f]
                   [plot-y-label  "milliseconds"])
      (plot (points (map vector xs t)
                    #:sym 'fullcircle1
                    #:color "blue"
                    #:line-width 2
                    #:x-min 0 #:x-max (+ 1 (length t))
                    #:y-min 0 #:y-max 15.0))))
    (displayln (stats->string t 6)))
