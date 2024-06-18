#lang racket

(require sketching)
(require "../libs/timing.rkt")
(require "../libs/draw-utils.rkt")
(require "../libs/randomness.rkt")
(require "../libs/cl-counters.rkt")

(provide min-blob-dist
         Blob-ID
         sq-unit
         unit
         RND
         random-integer
         get-rand-color
         get-color
         get-r-factor
         get-r-factor-f)

(define min-blob-dist 12)
(define Blob-ID (cl-count 0))
(define sq-unit 15)
(define unit  40)

; (define RND rand-bm-ivl)
(define RND random)

(define-syntax random-integer
  (syntax-rules ()
    ((_ max-value) (number->int (RND max-value)))))


(define bl-colors (vector-immutable "Wheat" "Yellow" "#ff3030" "Green" "#1e90ff"))
(define bl-colors-len (vector*-length bl-colors))
(define get-rand-color
  (lambda ()
    (vector-ref bl-colors (random-integer bl-colors-len))))

(define get-color
  (lambda(x cut-x y cut-y)
    (cond
      [(< x cut-x)
       (if (< y cut-y)
           "#ff3030"
           "Wheat")]
      [(< y cut-y) "#1e90ff"]
      [else "#1eff90"])))

(define-syntax get-r-factor
  (syntax-rules ()
    ((_ from to) (RND from to))))

(define-syntax get-r-factor-f
  (syntax-rules ()
    ((_ from to)
     (let ((val (RND from to))
           (val-sign (RND -1.0 1.0)))
       (if (< val-sign 0)
           (- val)
           val)))))