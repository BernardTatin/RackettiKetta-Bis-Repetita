#lang racket

(provide lazy-car
            lazy-cons
            lazy-cdr
            lazy-filter
            lazy-map
            lazy-ref
            head)

;;;;; basic functions and a macro

;;; car for lazy evaluation
(define lazy-car car)

;;; cdr for lazy evaluation
(define (lazy-cdr ls)
  (force (cdr ls)))

;;; lazy cons
(define-syntax lazy-cons
  (syntax-rules ()
    ((_ a b) (cons a (delay b)))))

;;; lazy map
(define (lazy-map fn . lss)
  (if (memq '() lss)
      '()
      (lazy-cons (apply fn (map lazy-car lss))
                 (apply lazy-map fn (map lazy-cdr lss)))))

;;; lazy filter
(define (lazy-filter pred ls)
  (if (null? ls)
      '()
      (let ((obj (lazy-car ls)))
        (if (pred obj)
            (lazy-cons obj  (lazy-filter pred (lazy-cdr ls)))
            (lazy-filter pred (lazy-cdr ls))))))

;;; returns n-th item of the lazy list
(define (lazy-ref ls n)
  (if (= n 0)
      (lazy-car ls)
      (lazy-ref (lazy-cdr ls) (- n 1))))

;;; returns first n items of the ls
(define (head ls n)
  (if (= n 0)
      '()
      (cons (lazy-car ls) (head (lazy-cdr ls) (- n 1)))))