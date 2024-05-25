#lang typed/racket

;; typed racket est une bonne idée
;; mais c'est un peu chiant!

(define str->int
  (lambda(str)
    (let ((z (string->number str)))
      (inexact->exact z))))


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


(define (main args)
    (cond
        [(null? args) (printf "usage: try-swindle [from] to, where from and to are integrs")]
        [(string=? "-h" (car args)) (main '())]
        [(null? (cdr args)) (show-fact 0 (str->int (car args)))]
        [#t      (show-fact (str->int (car args)) (str->int (car (cdr args))))]
    ))

(display "run, baby run!\n")
(let ((start (current-milliseconds)))
  (main (vector->list (current-command-line-arguments)))
  (display (format "tim: ~a ms~%" (- (current-milliseconds) start))))
