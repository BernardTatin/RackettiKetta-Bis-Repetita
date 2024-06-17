#lang sketching

(require "../libs/draw-utils.rkt")
(require "../libs/randomness.rkt")

; (define RND rand-bm-ivl)
(define RND random)

(define-syntax random-integer
    (syntax-rules ()
        ((_ max-value) (number->int (random max-value)))))

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
    ; (RND from to)))
    (let ((val (RND from to))
          (val-sign (RND -1.0 1.0)))
      (if (< val-sign 0)
          (- val)
          val))))

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
      (:= y ny))
    ; (let* ((n-speed (* (get-r-factor 0.8 1.2) speed x-dir))
    ;        (nx (+ x n-speed)))
    ;   (cond
    ;     [(<= nx sq-unit)
    ;       (:= x-dir  1)
    ;       (:= x (max sq-unit (+ x (abs n-speed))))]
    ;     [(>= nx  (- width sq-unit))
    ;       (:= x-dir -1)
    ;       (:= x (min (- width sq-unit) (- x (abs n-speed))))]
    ;     [else (:= x nx)]))
    ; (let* ((n-speed (* (get-r-factor 0.8 1.2) speed y-dir))
    ;        (ny (+ y n-speed)))
    ;   (cond
    ;     [(<= ny sq-unit)
    ;       (:= y-dir  1)
    ;       (:= y (max sq-unit (+ y (abs n-speed))))]
    ;     [(>= ny (- height sq-unit))
    ;       (:= y-dir -1)
    ;       (:= y (min (- height sq-unit) (- y (abs n-speed))))]
    ;     [else (:= y ny)]))
        )


  (define/public (draw)
    (fill bl-color)
    (ellipse x y 9 9)))

;;; ---------------

(define unit  40)
(define blobs (vector))

(define create-blobs
  (lambda(width height)
    (:= blobs (for*/vector  ; for* is a nested loop
                           ([xx (number->int (/ width  unit))]
                            [yy (number->int (/ height unit))])
                         (new Blob
                              [x        (+ (* xx unit) (* 1/2 unit))]
                              [y        (+ (* yy unit) (* 1/2 unit))]
                              [bl-color (if (< xx (* 1/2 (/ width unit))) "#ff3030" "#1e90ff")]
                              )))))

(define (setup)
  (size 640 800)
  ;; 60 fps: 1.0 processeur, mouvement très fluides
  ;; 30 fps: 0.6 processeur, mouvements légèrement saccadés
  ;; =========== c'est quand même mieux que l'Amstrad CPC 64
  (frame-rate 90)
  (no-stroke)
  ; (color-mode 'hsb)
  (create-blobs width height))

(define (on-resize width height)
    (create-blobs width height))

(define (draw)
  (background "#404040")
  (stroke-weight 4)
  (stroke "#aaaa44")
  (line (* 1/2 width) 0 (* 1/2 width) height)
  (no-stroke)
  (for ([blob blobs])
    (blob.update))
  (for ([blob blobs])
    (blob.draw)))
