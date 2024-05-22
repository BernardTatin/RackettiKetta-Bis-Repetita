#lang sketching

;; from https://docs.racket-lang.org/manual-sketching/Examples.html#%28part._examples_typography%29
;; with some modifications

; Letters.

;  Draws letters to the screen.

(define new-size #t)    ;; set when window size changes
(define cpt 0)          ;; count how many times wi call draw
(define cpt-ok? #f)     ;; enable console visualization of preceding counter

(define t-size 12)
(define gap    (- (* 2 t-size) 2))

(define (setup)
;  (size 1600 960)
  (size 640 360)
  (background 0)

  (text-weight 100)
;  (text-family 'swiss)
;  (text-face "Noto Sans Light")
  (text-face "Source Sans Pro ExtraLight")
  (text-size t-size)
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
  (translate (* 3 margin) (* 3 margin))

  (define counter 35)

;; <BT>
;; when resizing the window, it's necessary to erase the background
    (when new-size
        (black-back))
    (when cpt-ok?
        (+= cpt 1)
        (display (format "cpt ~a~%" cpt)))
;; </BT>
  (for ([y (in-range 0 (- height gap) gap)])
    (for ([x (in-range 0 (- width gap) gap)])
      ; Current letter
      (define letter (char counter))
      ; Color wovel and consonants differently
      (case (char-downcase letter)
        [(#\a #\e #\i #\o #\u) (fill 204 32 32)]
        [else                  (fill 255)])
      ; Draw the letter
      (text letter x y)
      ; Increment the counter
      (++ counter))))