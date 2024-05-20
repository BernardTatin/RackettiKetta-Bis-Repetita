#lang racket

(require bitwise-ops)
(provide gbits 2b)


(define gbits
  (lambda(value shift mask)
    ; (>> value shift)))
    (& (>> value shift) mask)))

(define 2b
  (lambda(x)
    (& x #xff)))