#lang sketching

(require "../libs/draw-utils.rkt")

(require "particles-lib.rkt")


(define blob-object
  (lambda(x y bl-color)
    (let ((ID               (Blob-ID))
          (ex               (RND 6 12))
          (ey               (RND 6 12))
          (vx            (get-r-factor-f 0.8 1.2))
          (vy            (get-r-factor-f 0.8 1.2)))
      (letrec ((the-blob
                (case-lambda
                  [(field)
                   (case field
                     [(pos)       (values x y)]
                     [(speed)     (values vx vy)]
                     [(shape)     (values ex ey)]
                     [(ID)        ID]
                     [(bl-color)  bl-color]
                     [else         #f])]
                  [(field v1 v2)
                   (case field
                     [(pos)     (:= x     v1) (:= y     v2)]
                     [(speed)   (:= vx v1) (:= vy v2)])]
                  )))
        the-blob))))

(define get-new-speed
  (lambda(v)
    (let* ((nv (* (get-r-factor 0.9 1.1) v))
           (anv (abs nv)))
      (cond
        [(< anv 0.8) (if (< nv 0) -1.0 1.0)]
        [(> anv 1.2) (if (< nv 0) -1.0 1.0)]
        [else nv]))))

(define-syntax move-p
  (syntax-rules ()
    ((_ p speed pmax)
     (let* ((n-speed (get-new-speed speed))
            (a-speed (abs n-speed))
            (np (+ p n-speed)))
       (cond
         [(< np sq-unit)
          (values a-speed (+ p a-speed))]
         [(> np  (- pmax sq-unit))
          (values (- a-speed)  (- p a-speed))]
         [else
          (values n-speed np)])))))

(define move-blob
  (lambda(bl)
    (let-values (((x y) (bl 'pos))
                 ((vx vy) (bl 'speed)))
        (let-values (((nvx nx) (move-p x vx width))
                     ((nvy ny) (move-p y vy height)))
          (bl 'pos  nx  ny)
          (bl 'speed nvx nvy)))))


(define blob-near?
  (lambda(bl1 bl2)
    (let-values (((x1 y1) (bl1 'pos))
                 ((ex1 ey1) (bl1 'shape))
                 ((x2 y2) (bl2 'pos))
                 ((ex2 ey2) (bl2 'shape)))

      (and ;; (not (= (bl1 'ID) (bl2 'ID)))
           (< (abs (- x1 x2)) (max ex1 ex2))
           (< (abs (- y1 y2)) (max ey1 ey2))))))

(define on-collision
  (lambda(bl1 bl2)
    (let-values (((xd1 yd1) (bl1 'speed))
                 ((xd2 yd2) (bl2 'speed)))
      (bl1 'speed xd2 yd2)
      (bl2 'speed xd1 yd1))))

(define search-colide-0
  (lambda()
    (let ((vl (vector-length blobs)))
      (for ([i (in-range 0 (- vl 1))])
        (let ((bl1 (vector-ref blobs i)))
          (for ([j (in-range (+ 1 i) vl)])
            (let ((bl2 (vector-ref blobs j)))
              (when (blob-near? bl1 bl2)
                (on-collision bl1 bl2)))))))))

(define search-colide
  (lambda()
    (let ((vl (vector-length blobs)))
      (letrec ((next-i
                (lambda(i j bl)
                  (when (< i (- vl 2))
                    (iloop (+ i 1) 0 #f))))
               (next-j
                (lambda(i j bl)
                  (if (< j (- vl 1))
                      (iloop i (+ 1 j) bl)
                      (next-i i j bl))))
               (iloop
                (lambda(i j bl)
                  (let* ((bl1 (if (not bl) (vector-ref blobs i) bl))
                         (bl2 (vector-ref blobs j))
                         (near? (blob-near? bl1 bl2)))
                    (if near?
                        (begin
                          (on-collision bl1 bl2)
                          (next-i i j bl))
                        (next-j i j bl))))))
        (iloop 0 1 #f)))))


(define draw-blob
  (lambda(bl)
    (let ((color (bl 'bl-color)))
      (let-values (((x   y) (bl 'pos))
                   ((ex ey) (bl 'shape)))
        (fill color)
        (ellipse x y ex ey)))))

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
                    (blob-object nx ny (get-color nx cut-x ny cut-y))))))))

(define (setup)
  (size 640 800)
  (frame-rate 110)
  (no-stroke)
  (create-blobs width height))

(define (on-resize width height)
  (create-blobs width height))

(define now #f)
(define frames 0)

(define move-count -1)
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
  (search-colide)
  (for ([blob blobs])
    (move-blob blob)
    (draw-blob blob)))

