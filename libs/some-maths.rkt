#lang racket

(provide identity
    proj-X proj-Y
    safe-inv safe-inv-1
    pythagore pythagore-2)

(define identity
  (lambda(x)
    x))


(define proj-X
  (lambda (X Y)
    X))

(define proj-Y
  (lambda (X Y)
    Y))

(define safe-inv
  (lambda(x bad)
    (if (= 0 x)
      bad
      (/ 1.0 x))))

(define safe-inv-1
  (lambda(x)
    (safe-inv x 1)))

(define pythagore-2
  (lambda(x y)
    (+ (* x x) (* y y))))

(define pythagore
  (lambda(x y)
    (sqrt (pythagore-2 x y))))