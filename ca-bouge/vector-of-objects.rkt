#lang sketching

(require "../libs/draw-utils.rkt")

;; from https://github.com/soegaard/sketching/blob/main/sketching-doc/sketching-doc/manual-examples/basics/vectors/vector-of-objects.rkt
;; but with more random

; https://github.com/processing/processing-docs/tree/master/content/examples/Basics/Arrays/ArrayObjects
; Vector of objects.



(define bl-colors (vector-immutable "White" "Yellow" "Red" "Green" "Blue"))
(define bl-colors-len (vector*-length bl-colors))
(define get-rand-color
  (lambda ()
    (vector-ref bl-colors (random-integer bl-colors-len))))

(define get-r-factor
  (lambda(from to)
    (let ((val (random from to))
          (val-sign (random -1.0 1.0)))
      (if (< val-sign 0)
          (- val)
          val))))

; Demonstrates how to create a vector of objects.

(class Blob Object
  ; "Constructor"
  (init-field [x-offset 0]
              [y-offset 0]
              [x        0]
              [y        0]
              [unit     1]
              [x-dir    1]
              [y-dir    1]
              [speed    1]
              [bl-color (get-rand-color)])
  (super-new)    ; super class initialization

  ; Methods
  (define/public (update)
    (:= x (+ x (* (get-r-factor 0.7 1.0) speed x-dir)))
    (unless (<= 0 x unit)
      (*= x-dir -1)
      (:= x (+ x x-dir)))

    (:= y (+ y (* (get-r-factor 0.7 1.0) speed y-dir)))
    (unless (<= 0 y unit)
      (*= y-dir -1)
      (:= y (+ y y-dir))))

  (define/public (draw)
    (fill bl-color)
    (ellipse (+ x x-offset) (+ y y-offset) 9 9)))

;;; ---------------

(define unit  40)
(define blobs (vector))

(define create-blobs
  (lambda(width height)
    (:= blobs (for*/vector  ; for* is a nested loop
                           ([x (inexact->exact (floor (/ width  unit)))]
                            [y (inexact->exact (floor (/ height unit)))])
                         (new Blob
                              [x-offset (* x   unit)]
                              [y-offset (* y   unit)]
                              [x        (inexact->exact (* 1/2 unit))]
                              [y        (inexact->exact (* 1/2 unit))]
                              [speed    (random 0.5 1.0)]
                              [unit     (random 0.8 1.6)])))))

(define (setup)
  (size 643 367)
  (frame-rate 60)
  (no-stroke)
  (create-blobs width height))

(define (on-resize width height)
    (create-blobs width height))

(define (draw)
  (background 0)
  (for ([blob blobs])
    (blob.update)
    (blob.draw)))
