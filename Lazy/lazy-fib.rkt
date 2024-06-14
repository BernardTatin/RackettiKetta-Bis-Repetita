#lang racket

;; https://www.shido.info/lisp/scheme_lazy_e.html

(require "./lazy-lib.rkt")

(define lazy-fib
  (lazy-cons 1
             (lazy-cons 1
                        (lazy-map + lazy-fib (lazy-cdr lazy-fib)))))

(head lazy-fib 20)
(lazy-ref lazy-fib 100)