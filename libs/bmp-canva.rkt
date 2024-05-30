#lang racket/gui

(provide bmp-canvas<%>
         bmp-canvas%)

(define bmp-canvas<%>
  (interface ()
    create-bitmap
    fill-pixels
    do-paint))

(define bmp-canvas%
  (class* canvas% (bmp-canvas<%>)
    (field (bitmap #f))
    (super-new (paint-callback
                (lambda (c dc) (my-paint-callback c dc))))

    (define/public create-bitmap
      (lambda(width height)
        (make-bitmap width height)))

    (define/override on-size
      (lambda(width height)
        (let ((bmp (create-bitmap width height)))
          (let ((pixels (fill-pixels width height)))
            (send bmp set-argb-pixels 0 0 width height pixels))
          (set! bitmap bmp)
          (send this refresh-now))))

    (define/public do-paint
      (lambda(dc)
        (when bitmap
          (send dc draw-bitmap bitmap 0 0))))

    ;; -------------------------------------------------------
    ( define/public (my-paint-callback myself dc)
       (do-paint dc))

    (abstract fill-pixels)))