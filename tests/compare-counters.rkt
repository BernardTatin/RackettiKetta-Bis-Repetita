#lang racket

(require math/statistics)
(require plot)
(require plot/utils)

; (require "../libs/rolling-cpt.rkt")
(require "../libs/cl-counters.rkt")
(require "../libs/timing.rkt")

(define nloops 250000)
(define chrono-loops 1500)

(define kill-max
  (lambda (lst le-max)
    (foldl (lambda(e l)
              (if (<= e le-max)
                (cons e l)
                l))
                '() lst)))

(define plot-stats
  (lambda(name in-stats)
    (let*  ((y-0.9 (quantile 0.9 < in-stats))
            (stats (kill-max in-stats y-0.9))
            (ymax  (* 1.2 y-0.9))
            (y.1 (quantile 0.1 < stats))
            (y.9 (quantile 0.9 < stats))
            (l-stats (length stats))
            (xs (range 0 l-stats))
            (vstats (map vector xs stats)))
      (plot-new-window? #t)
      (plot-title name)
      (parameterize ([plot-width    550]
                     [plot-height   550]
                     [plot-x-label  name]
                     [plot-y-label  "milliseconds"])
        (plot (list
               (lines-interval (list (vector 0 y.1) (vector (- l-stats 1) y.1))
                               (list (vector 0 y.9) (vector (- l-stats 1) y.9))
                               #:color (->pen-color 4)
                               ;  #:line1-color (->pen-color 4) #:line2-color (->pen-color 4)
                               #:label #f)
               (lines vstats
                      #:marker 'fullcircle1
                      #:color "blue"
                      #:width 2
                      #:x-min 0 #:x-max (+ 1 (length stats))
                      #:y-min 0 #:y-max ymax))))
      (values ymax name vstats)
      )))

(define-syntax do-test
  (syntax-rules ()
    ((_ (name) body ...)
     (let ((stats (with-stats-timing (chrono-loops)
                    (for ((i (in-range 0 nloops)))
                      body ...))))
       (printf "~a : ~a\n" (~a name #:min-width 11)
               (stats->string stats))
       (plot-stats name stats)
       ))))

(let  ((l-colors '("blue" "red" "green"))
       (all-stats '())
       (all-names '())
       (ymax 0))

  ;; a define-syntax would bz a good idea
  (let ((cpt (cl-count 0 1)))
    (let-values (((ym name lst) (do-test ("cl-count") (cpt))))
      (set! ymax (max ym ymax))
      (set! all-names (cons name all-names))
      (set! all-stats (cons lst  all-stats))))

  (let ((cpt (cl-count+ 0 1)))
    (let-values (((ym name lst) (do-test ("cl-count+") (cpt 'next))))
      (set! ymax (max ym ymax))
      (set! all-names (cons name all-names))
      (set! all-stats (cons lst  all-stats))))

  (let ((cpt (cl-rcount-2d 0 800 0 640)))
    (let-values (((ym name lst) (do-test ("cl-count-2d") (cpt))))
      (set! ymax (max ym ymax))
      (set! all-names (cons name all-names))
      (set! all-stats (cons lst  all-stats))))

  (plot-new-window? #t)
  (plot-title "All stats")


  (define lines-maker-map
    (lambda(lst)
      (map (lambda(e color name)
             (lines e
                    #:marker 'fullcircle1
                    #:color color
                    #:width 2
                    #:label name
                    #:x-min 0 #:x-max (+ 1 (length e))
                    #:y-min 0 #:y-max ymax))
           lst l-colors all-names
           )))

  (parameterize ([plot-width    550]
                 [plot-height   550]
                 [plot-y-label  "milliseconds"])

    (plot (lines-maker-map all-stats)))
  )

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

