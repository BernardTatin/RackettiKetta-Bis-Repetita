#lang racket

(require sketching)
(require "../libs/draw-utils.rkt")

(define frames 0)
(define chrono
  (lambda(limit on-read on-limit)
    (let ((now (current-inexact-milliseconds)))
      (let ((ichrono
             (lambda()
               (let ((dt (- (current-inexact-milliseconds) now)))
                 (if (< dt limit)
                     (on-read)
                     (begin
                       (on-limit dt)
                       (:= now (current-inexact-milliseconds))))))))
        ichrono))))


(define frame-chrono-0
  (lambda(limit)
    (chrono limit
            (lambda()
              (++ frames))
            (lambda(dt)
              (display-all "fps: " (/ (* 1000 frames)  dt) "\n")
              (:= frames 0)))))

(define frame-chrono
  (lambda(limit)
    (let* ((frames 0)
           (on-read
            (lambda()
              (++ frames)))
           (on-tick
            (lambda(dt)
              (display-all "---- fps: " (/ (* 1000 frames)  dt) "\n")
              (:= frames 0))))
      (chrono limit on-read on-tick))))

(define the-frame-chrono-0 (frame-chrono-0 1500))
(define the-frame-chrono (frame-chrono 1500))


(the-frame-chrono-0)
(the-frame-chrono-0)
(the-frame-chrono-0)
(the-frame-chrono-0)
(the-frame-chrono-0)

(the-frame-chrono)
(the-frame-chrono)
(the-frame-chrono)
(the-frame-chrono)
(the-frame-chrono)