#lang racket

(require plot)
(plot-new-window? #t)
(plot-title "random")

(parameterize ([plot-width    150]
                 [plot-height   150]
                 [plot-x-label  #f]
                 [plot-y-label  #f])
    (define xs (build-list 20 (λ _ (random))))
    (define ys (build-list 20 (λ _ (random))))
    (list (plot (points (map vector xs ys)))
          (plot (points (map vector xs ys)
                        #:x-min 0 #:x-max 1
                        #:y-min 0 #:y-max 1))))
                        
