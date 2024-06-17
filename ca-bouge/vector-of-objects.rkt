#lang sketching

(require "../libs/timing.rkt")
(require "../libs/draw-utils.rkt")
(require "../libs/randomness.rkt")

(define RND rand-bm-ivl)
; (define RND random)

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

(define get-r-factor
  (lambda(from to)
    (RND from to)))
    ; (let ((val (RND from to))
    ;       (val-sign (RND -1.0 1.0)))
    ;   (if (< val-sign 0)
    ;       (- val)
    ;       val))))

; Demonstrates how to create a vector of objects.
(define sq-unit 15)

(class Blob Object
  ; "Constructor"
  (init-field [x        0]
              [y        0]
              [x-dir    1]
              [y-dir    1]
              [speed    (RND 0.8 1.2)]
              [bl-color (get-rand-color)])
  (super-new)    ; super class initialization

  (define/private (move p p-dir pmax)
    (let* ((n-speed (* (get-r-factor 0.8 1.2) speed p-dir))
           (np (+ p n-speed)))
      (cond
        [(<= np sq-unit)
          (values 1 (max sq-unit (+ p (abs n-speed))))]
        [(>= np  (- pmax sq-unit))
          (values -1 (max sq-unit (- p (abs n-speed))))]
        [else
          (values p-dir np)])))

; Methods
  (define/public (update)
    (let-values (((n-dir nx) (move x x-dir width)))
      (:= x-dir n-dir)
      (:= x nx))
    (let-values (((n-dir ny) (move y y-dir height)))
      (:= y-dir n-dir)
      (:= y ny)))


  (define/public (draw)
    (fill bl-color)
    (ellipse x y 9 9)))

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
  (frame-rate 40)
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
  (for ([i (in-range 10)])
    (for ([blob blobs])
      (blob.update)))
  (for ([blob blobs])
    (blob.draw)))
