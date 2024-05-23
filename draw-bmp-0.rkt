#lang racket/gui

(require bitwise-ops)
(require "libs/bits-ops.rkt")
;; ======================================================================
;; some-maths-2.ss
;;
;; GRacket.exe -t .\some-maths-1.ss
;;
;; parts from https://stackoverflow.com/questions/7294117/racket-using-events-in-a-frame-window
;;
;; simple maths graphs
;; we know how to fill and show a bitmap,
;; now we will try some more mathematical things
;; ======================================================================

(define frame-w 800)
(define frame-h 640)
(define frame-title "Some Maths 2")

(define alpha-value 255)
(define BPPX 4)

(define my-canvas%
  (class canvas% ; The base class is canvas%
    ;; -------------------------------------------------------
    ; Call the superclass init, passing on all init args
    (super-new (paint-callback (lambda (c dc) (my-paint-callback c dc))))
    ;; -------------------------------------------------------
    ;; a set of fields
    [field (bitmap #f)
           ]

    (define/private create-bitmap
      (lambda(width height)
        (make-bitmap width height)))


    (define/private fill-pixels
      (lambda(width height)
        (let* ((i-max (* width height BPPX))
               ;; fill the bytes with 255, the alpha value
               (pixels (make-bytes i-max alpha-value)))
          (letrec ((i-fill
                    (lambda (i)
                      (define-values (y x) (quotient/remainder i width))
                      (when (< i i-max)
                        ;; alpha value already at 255
                        ; (bytes-set! pixels i 255)
                        (bytes-set! pixels (+ 1 i) (2b x))
                        (bytes-set! pixels (+ 2 i) (2b y))
                        (bytes-set! pixels (+ 3 i) (2b (quotient (+ x y) 2)))
                        (i-fill (+ i BPPX))))))
            (i-fill 0)
            pixels
            ))
        ))

    (define/override on-size
      (lambda(x y)
        (let ((bmp (create-bitmap x y)))
          (let ((pixels (fill-pixels x y)))
            (send bmp set-argb-pixels 0 0 x y pixels))
          (set! bitmap bmp)
          (send this refresh-now))))

    ;; -------------------------------------------------------
    [ define/private (my-paint-callback myself dc)
       ;  (send dc set-scale 1 1)
       (when bitmap
         (send dc draw-bitmap bitmap 0 0))
       ]
    ))

(define make-new-canvas
  (lambda(frame)
    (let ((the-canva (new my-canvas% [parent frame])))
      the-canva)))

(define my-frame%
  [class frame%
    ;; super-new must be called here if we want to use the this keyword
    ;; in the fields definition
    (super-new)
    (field (canva (make-new-canvas this)))])


(define my-app%
  (class object%
    (super-new)
    (field (frame  (new my-frame% [label frame-title]
                        (width frame-w) (height frame-h))))

    (define/public start
      (lambda()
        (send frame show #t)))))

(define app (new my-app%))
(send app start)

