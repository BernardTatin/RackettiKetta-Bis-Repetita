#lang racket

;; from https://alex-hhh.github.io/2018/03/interactive-overlays-with-the-racket-plot-package-update.html

(require racket/gui mrlib/snip-canvas plot)

(define from (- (* 5 pi)))
(define to      (* 5 pi))
(define ymin (min -2.1 from))
(define ymax (max  2.1 to))

(define compute-height
    (lambda(width height)
        (let ((r (/ (- to from) (- ymax ymin))))
            (inexact->exact (truncate (/ width r))))))

(define the-f
    (lambda(x)
        (+ (sin (* 2 x)) (cos (* 3 x)))))

(define ((make-current-value-renderer fn) snip event x y)
  (define overlays
    (and x y (eq? (send event get-event-type) 'motion)
         (list (vrule x #:style 'long-dash)
               (hrule y #:style 'long-dash)
               (point-label (vector x (fn x)) #:anchor 'auto))))
  (send snip set-overlay-renderers overlays))

(define (make-plot-snip width height)
  (define snip  (plot-snip (list
                            (axes)
                            (function the-f) )
                           #:x-min from #:x-max to #:y-min ymin #:y-max ymax
                           #:width width #:height height))
  (send snip set-mouse-event-callback (make-current-value-renderer the-f))
  snip)

(define toplevel (new frame% [label "Plot"] [width 500] [height 200]))
(define canvas (new snip-canvas% [parent toplevel] [make-snip make-plot-snip]))
(send toplevel show #t)