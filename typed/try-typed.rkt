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


(define from  0)
(define to   18)

(show-fact from to)