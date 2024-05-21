#lang sketching

;; from https://docs.racket-lang.org/manual-sketching/Examples.html#%28part._examples_typography%29
; Letters.

;  Draws letters to the screen.

(define new-size #t)

(define (setup)
;  (size 1600 960)
  (size 640 360)
  (background 0)

  (text-face "Tahoma")
  (text-size 24)
  (text-align 'center 'center))

;; <BT>
(define (on-resize width height)
    (set! new-size #t))

(define (black-back)
    (rect-mode 'corner)
    (fill "black")
    (rect 0 0 width height)
    (set! new-size #f))
;; </BT>

(define (draw)
  ; set the left and top marings
  (define margin 10)
  (translate (* 4 margin) (* 4 margin))

  (define gap     46)
  (define counter 35)

;; <BT>
;; when resizing the window, it's necessary to erase the background
    (when new-size
        (black-back))
;; </BT>
  (for ([y (in-range 0 (- height gap) gap)])
    (for ([x (in-range 0 (- width gap) gap)])
      ; Current letter
      (define letter (char counter))
      ; Color wovel and consonants differently
      (case (char-downcase letter)
        [(#\a #\e #\i #\o #\u) (fill 255 204 0)]
        [else                  (fill 255)])
      ; Draw the letter
      (text letter x y)
      ; Increment the counter
      (++ counter))))