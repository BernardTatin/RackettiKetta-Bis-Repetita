#lang typed/racket

;; typed racket est une bonne idée
;; mais c'est un peu chiant!


(: fact : Integer -> Integer)
(define (fact n)
  (letrec ((ifact : (Integer Integer -> Integer)
    (lambda (k acc)
                    (if (<= k 0)
                        acc
                        (ifact (- k 1) (* k acc))))))
    (ifact n 1)))

(: show-fact : Integer Integer -> Any)
(define (show-fact n to)
  (when (<= n to)
    (begin
      (display (format "~a! = ~a\n" n (fact  n)))
      (show-fact (+ n 1) to))))


(define from (make-parameter  0))
(define to   (make-parameter 18))

#|
  le défaut: impossible de se sortir du trou
  avec la doc uniquement
|#
; (define (from-sht-to-int a)
;   (let ((str (format "~a" a)))
;     (let ((z (string->number str)))
;       (inexact->exact z))))
; (define (main args)
;   (match args
;     ['() #f]
;     [(cons a b)
;       (from (from-sht-to-int a))
;       (to   (from-sht-to-int b))
;     ]))

(show-fact (from) (to))