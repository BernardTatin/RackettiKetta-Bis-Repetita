#lang racket

(require racket/contract)

(define/contract (maybe-invert i b)
    (-> integer? boolean? integer?)
    (if b (- i) i))

(define/contract (fact n)
    (-> exact-nonnegative-integer? integer?)
    (letrec ((ifact
        (lambda (k acc)
            (if (= k 0)
            acc
            (ifact (- k 1) (* k acc))))))

        (ifact n 1)))

(define show-fact
    (lambda(n)
        (with-handlers ([exn:fail:contract?
            (lambda(e)
            (format "Bad argument type (~a)~%" n))])
         (display (format "~a! = ~a~%" n (fact n))))))

(show-fact "Oh!")
(show-fact -1)
(show-fact "Oh!")
(for ([i (in-range 8)])
    (show-fact i))
