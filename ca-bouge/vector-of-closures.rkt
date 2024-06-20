#lang sketching

(require "../libs/draw-utils.rkt")

(require "particles-lib.rkt")


(define blob-object
  (lambda(x y bl-color)
    (let ((x-dir            1)
          (y-dir            1)
          (ID               (Blob-ID))
          (ex               (RND 6 12))
          (ey               (RND 6 12))
          (speed            (get-r-factor 0.8 1.2)))
      (letrec ((the-blob
                (case-lambda
                  [(field)
                   (case field
                     [(pos)       (values x y)]
                     [(dir)       (values x-dir y-dir)]
                     [(shape)     (values ex ey)]
                     [(ID)        ID]
                     [(speed)     speed]
                     [(bl-color)  bl-color]
                     [else         #f])]
                  [(field v1 v2)
                   (case field
                     [(pos)     (:= x     v1) (:= y     v2)]
                     [(dir)     (:= x-dir v1) (:= y-dir v2)])]
                  )))
        the-blob))))

(define-syntax move-p
  (syntax-rules ()
    ((_ p p-dir speed pmax)
     (let* ((n-speed (* (get-r-factor 0.8 1.2) speed p-dir))
            (a-speed (abs n-speed))
            (np (+ p n-speed)))
       (cond
         [(< np sq-unit)
          (values 1 (+ p a-speed))]
         [(> np  (- pmax sq-unit))
          (values -1  (- p a-speed))]
         [else
          (values p-dir np)])))))

(define move-blob
  (lambda(bl)
    (let-values (((x y) (bl 'pos))
                 ((x-dir y-dir) (bl 'dir)))
      (let ((speed (bl 'speed)))
        (let-values (((nxd nx) (move-p x x-dir speed width))
                     ((nyd ny) (move-p y y-dir speed height)))
          (bl 'pos  nx  ny)
          (bl 'dir nxd nyd))))))


(define blob-near?
  (lambda(bl1 bl2)
    (let-values (((x1 y1) (bl1 'pos))
                 ((ex1 ey1) (bl1 'shape))
                 ((x2 y2) (bl2 'pos))
                 ((ex2 ey2) (bl2 'shape)))

      (and (not (= (bl1 'ID) (bl2 'ID)))
           (< (abs (- x1 x2)) (min ex1 ex2))
           (< (abs (- y1 y2)) (min ey1 ey2))))))

(define on-collision
  (lambda(bl1 bl2)
    (let-values (((xd1 yd1) (bl1 'dir))
                 ((xd2 yd2) (bl2 'dir)))
      (bl1 'dir xd2 yd2)
      (bl2 'dir xd1 yd1))))

(define search-colide   ;; 46/47 fps
  (lambda()
    (let ((vl (vector-length blobs)))
      (for ([i (in-range 0 (- vl 1))])
        (let ((bl1 (vector-ref blobs i)))
          (for ([j (in-range (+ 1 i) vl)])
            (let ((bl2 (vector-ref blobs j)))
              (when (blob-near? bl1 bl2)
                (on-collision bl1 bl2)))))))))

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


(define move-count -1)
(define (draw)
  (the-frame-chrono)
  (background "#404040")
  (search-colide)
  (for ([blob blobs])
    (move-blob blob)
    (draw-blob blob)))

