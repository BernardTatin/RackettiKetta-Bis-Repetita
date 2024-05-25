#lang racket

(require swindle)

(define str->int
    (lambda(str)
        (let ((z (string->number str)))
            (inexact->exact z))))

(defgeneric (fact n))

(defmethod (fact [n <integer>])
  (letrec ((ifact
    (lambda (k acc)
                    (if (<= k 0)
                        acc
                        (ifact (- k 1) (* k acc))))))
    (ifact n 1)))

(defgeneric (show-fact from to))

(defmethod (no-applicable-method [m (singleton show-fact)] from to)
  (echo "No method in" m "for" :w from to))

(defmethod (show-fact [n <integer>] [to <integer>])
  (when (<= n to)
    (begin
      (display (format "~a! = ~a\n" n (fact  n)))
      (show-fact (+ n 1) to))))

(defmethod (show-fact [from <integer>] [to <string>])
    (show-fact from (str->int to)))

(defmethod (show-fact [from <string>] [to <string>])
    (show-fact (str->int from) (str->int to)))

(define (main args)
    (cond
        [(null? args) (printf "usage: try-swindle [from] to, where from and to are integrs")]
        [(string=? "-h" (car args)) (main '())]
        [(null? (cdr args)) (show-fact 0 (car args))]
        [#t      (show-fact (car args) (car (cdr args)))]
    ))


(display "run, baby run!\n")

(let ((start (current-milliseconds)))
  (main (vector->list (current-command-line-arguments)))
  (display (format "tim: ~a ms~%" (- (current-milliseconds) start))))
