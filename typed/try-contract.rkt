#lang racket

(require racket/contract)

(define str->int
  (lambda(str)
    (let ((z (string->number str)))
      (inexact->exact z))))

(define/contract (fact n)
  (-> exact-nonnegative-integer? integer?)
  (letrec ((ifact
            (lambda (k acc)
              (if (= k 0)
                  acc
                  (ifact (- k 1) (* k acc))))))

    (ifact n 1)))


(define show-fact
  (lambda(n to)
    (when (<= n to)
      (begin
        (with-handlers ([exn:fail:contract?
                         (lambda(e)
                           (format "Bad argument type (~a)~%" n))])
          (display (format "~a! = ~a~%" n (fact n))))
        (show-fact (+ n 1) to)))))

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
