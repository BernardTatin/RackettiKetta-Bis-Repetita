#lang racket/gui

; (require bitwise-ops)
(require "../libs/rolling-cpt.rkt")
(require "../libs/bits-ops.rkt")
(require "../libs/bmp-canva.rkt")

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
  (class* bmp-canvas% ()
    ;; -------------------------------------------------------
    ; Call the superclass init, passing on all init args
    (super-new)
    ;; -------------------------------------------------------
    ;; a set of fields
    [field (zmin 0)
           (zmax 0)
           (cmin 0)
           (cmax #xffffff)]

    (define/private get-z
      (lambda(x y)
        x))

    (define/override create-bitmap
      (lambda(width height)
        (set! zmin (get-z 0 0))
        (set! zmax (get-z width height))
        (super create-bitmap width height)))

    (define/private normalize-z
      (lambda(z)
        (cond
          ((= zmin zmax) 0)
          (else
           ;; mise à l'échelle:
           ;;  (c - cmin)       (z - zmin)
           ;; ------------- = -------------
           ;; (cmax - cmin)   (zmax - zmin)
           ;;
           ;;            (cmax - cmin) * (z - zmin)
           ;; c = cmin + --------------------------
           ;;                   (zmax - zmin)                       (zmax - zmin)
           (let ((nz (quotient
                      (* (- cmax cmin) (- z zmin))
                      (- zmax zmin))))
             (inexact->exact (+ nz cmin)))))))

    (define/override fill-pixels
      (lambda(width height)
        (let* ((i-max (* width height BPPX))
               ;; fill the bytes with 255, the alpha value
               (pixels (make-bytes i-max alpha-value))
               (xy (new rolling-cpt-2d% [xmax width] [ymax height]))
               )
          (letrec ((i-fill
                    (lambda (i x y)
                      (when (< i i-max)
                        (let* ((z (normalize-z (get-z x y))))
                          ;; alpha value already at 255
                          ; (bytes-set! pixels i 255)
                          (bytes-set! pixels (+ 1 i) (gbits z  0 #xff))
                          (bytes-set! pixels (+ 2 i) (gbits z  8 #xff))
                          (bytes-set! pixels (+ 3 i) (gbits z 16 #xff))
                          (let-values (((xx yy) (send xy next)))
                            (i-fill (+ i BPPX) xx yy)))))))
            (let-values (((xx yy) (send xy get)))
              (i-fill 0 xx yy))
            pixels))
        ))))

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


