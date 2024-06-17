#lang sketching

(require "../libs/timing.rkt")
(require "../libs/draw-utils.rkt")
(require "../libs/randomness.rkt")
(require "../libs/cl-counters.rkt")

; (define RND rand-bm-ivl)
(define RND random)

(define-syntax random-integer
  (syntax-rules ()
    ((_ max-value) (number->int (RND max-value)))))

;; from https://github.com/soegaard/sketching/blob/main/sketching-doc/sketching-doc/manual-examples/basics/vectors/vector-of-objects.rkt
;; but with more random

; https://github.com/processing/processing-docs/tree/master/content/examples/Basics/Arrays/ArrayObjects
; Vector of objects.



(define bl-colors (vector-immutable "Wheat" "Yellow" "#ff3030" "Green" "#1e90ff"))
(define bl-colors-len (vector*-length bl-colors))
(define get-rand-color
  (lambda ()
    (vector-ref bl-colors (random-integer bl-colors-len))))

(define-syntax get-r-factor-f
  (syntax-rules ()
    ((_ from to) (RND from to))))

(define-syntax get-r-factor
  (syntax-rules ()
    ((_ from to)
     (let ((val (RND from to))
           (val-sign (RND -1.0 1.0)))
       (if (< val-sign 0)
           (- val)
           val)))))

(define sq-unit 15)
(define-syntax move-p
  (syntax-rules ()
    ((_ p p-dir speed pmax boom)
     (let* ((n-speed (* (get-r-factor 0.8 1.2) speed p-dir))
            (np (+ p n-speed)))
       (cond
         [(<= np sq-unit)
          (:= p-dir 1)
          (:= p (max sq-unit (+ p (abs n-speed))))]
         [(>= np  (- pmax sq-unit))
          (:= p-dir -1)
          (:= p (min (- pmax sq-unit) (+ p (abs n-speed))))]
         [else
          (unless (not boom)
            (:= p-dir (- p-dir)))
          (:= p np)])))))


(define min-blob-dist 12)
(define Blob-ID (cl-count 0))
; Demonstrates how to create a vector of objects.
(class Blob Object
  ; "Constructor"
  (init-field [x        0]
              [y        0]
              [x-dir    1]
              [y-dir    1]
              [boom     #f]
              [ID       (Blob-ID)]
              [ex       (RND 6 12)]
              [ey       (RND 6 12)]
              [speed    (RND 0.8 1.2)]
              [bl-color (get-rand-color)])
  (super-new)    ; super class initialization

  ; Methods
  (define/public (update)
    (move-p x x-dir speed width  boom)
    (move-p y y-dir speed height boom)
    (unless (not boom)
      (reset-boom)
      ; provoque une erreur
      ; (boom.reset-boom)
      ))

  (define/public (on-collision bl)
    (:= boom bl))

  (define/public (reset-boom)
    (:= boom #f))

  (define/public (draw)
    (fill bl-color)
    (ellipse x y ex ey)))

(define blob-near?
  (lambda(bl1 bl2)
    (and (not (= bl1.ID bl2.ID))
         (< (abs (- bl1.x bl2.x)) min-blob-dist)
         (< (abs (- bl1.y bl2.y)) min-blob-dist))))

(define search-colide
  (lambda()
    (for ([bl1 blobs])
      (for ([bl2 blobs])
        (when (blob-near? bl1 bl2)
          (bl1.on-collision bl2))))))
;;; ---------------

(define unit  40)
(define blobs (vector))

(define get-color
  (lambda(x cut-x y cut-y)
    (cond
      [(< x cut-x)
       (if (< y cut-y)
           "#ff3030"
           "#8090ff")]
      [(< y cut-y) "#1e90ff"]
      [else "#1eff90"])))

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
  ; (color-mode 'hsb)
  (create-blobs width height))

(define (on-resize width height)
  (create-blobs width height))

(define now #f)
(define frames 0)

(define (draw)
  (cond
    [(not now)
     (:= now (current-inexact-milliseconds))
     (:= frames 0)]
    [else
     (let ((dt (- (current-inexact-milliseconds) now)))
       (++ frames)
       (when (> dt 10000.0)
         (display-all "fps: " (/ (* 1000 frames)  dt) "\n")
         (:= now (current-inexact-milliseconds))
         (:= frames 0)))])

  (background "#404040")
  (stroke-weight 4)
  (stroke "#aaaa44")
  (line (* 1/2 width) 0 (* 1/2 width) height)
  (line 0 (* 1/2 height) width (* 1/2 height))
  (no-stroke)
  (search-colide)
  (for ([blob blobs])
    (blob.update))
  ; (for ([i (in-range 10)])
  ;   (for ([blob blobs])
  ;     (blob.update)))
  (for ([blob blobs])
    (blob.draw)))
