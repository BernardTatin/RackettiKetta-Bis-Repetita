#lang sketching

(require "../libs/timing.rkt")
(require "../libs/draw-utils.rkt")
(require "../libs/randomness.rkt")
(require "../libs/cl-counters.rkt")

(require "particles-lib.rkt")

;; from https://github.com/soegaard/sketching/blob/main/sketching-doc/sketching-doc/manual-examples/basics/vectors/vector-of-objects.rkt
;; but with more random

; https://github.com/processing/processing-docs/tree/master/content/examples/Basics/Arrays/ArrayObjects
; Vector of objects.




(define-syntax move-p
  (syntax-rules ()
    ((_ p p-dir speed pmax)
     (let* ((n-speed (* (get-r-factor 0.8 1.2) speed p-dir))
            (np (+ p n-speed)))
       (cond
         [(<= np sq-unit)
          (:= p-dir 1)
          (:= p (max sq-unit (+ p (abs n-speed))))]
         [(>= np  (- pmax sq-unit))
          (:= p-dir -1)
          (:= p (min (- pmax sq-unit) (- p (abs n-speed))))]
         [else
          (:= p np)])))))


; Demonstrates how to create a vector of objects.
(class Blob Object
  ; "Constructor"
  (init-field [x        0]
              [y        0]
              [x-dir    1]
              [y-dir    1]
              [ID       (Blob-ID)]
              [ex       (RND 6 12)]
              [ey       (RND 6 12)]
              [speed    (RND 0.8 1.2)]
              [bl-color (get-rand-color)])
  (super-new)    ; super class initialization

  ; Methods
  (define/public (update)
    (move-p x x-dir speed width)
    (move-p y y-dir speed height))

  (define/public (on-collision bl)
    (let ((bl-xd bl.x-dir)
          (bl-yd bl.y-dir)
          (me-xd x-dir)
          (me-yd y-dir))
      (:= x-dir bl-xd)
      (:= y-dir bl-yd)
      (:= bl.x-dir me-xd)
      (:= bl.y-dir me-yd)))

  (define/public (draw)
    (fill bl-color)
    (ellipse x y ex ey)))

(define blob-near?
  (lambda(bl1 bl2)
    (and (not (= bl1.ID bl2.ID))
         (< (abs (- bl1.x bl2.x)) (min bl1.ex bl2.ex))
         (< (abs (- bl1.y bl2.y)) (min bl1.ey bl2.ey)))))

(define search-colide   ;; 46/47 fps
  (lambda()
    (let ((vl (vector-length blobs)))
      (for ([i (in-range 0 (- vl 1))])
        (let ((bl1 (vector-ref blobs i)))
          (for ([j (in-range (+ 1 i) vl)])
            (let ((bl2 (vector-ref blobs j)))
              (when (blob-near? bl1 bl2)
                (bl1.on-collision bl2)))))))))

;;; ---------------

(define blobs (vector))

(define create-blobs
  (lambda(width height)
    (let ((cut-x (* 1/2 width))
          (cut-y (* 1/2 height)))
      (:= blobs (for*/vector  ; for* is a nested loop
                    ([xx (number->int (/ width  unit))]
                     [yy (number->int (/ height unit))])
                  (let ((nx (+ (* xx unit) (* 1/2 unit)))
                        (ny (+ (* yy unit) (* 1/2 unit))))
                    (new Blob
                         [x        nx]
                         [y        ny]
                         [bl-color (get-color nx cut-x ny cut-y)]
                         )))))))

(define (setup)
  (size 640 800)
  (frame-rate 110)
  (no-stroke)
  (create-blobs width height))

(define (on-resize width height)
  (create-blobs width height))

(define (draw)
  (the-frame-chrono)
  (background "#404040")
  (search-colide)
  (for ([blob blobs])
    (blob.update)
    (blob.draw)))
