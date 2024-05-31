#lang racket

(define-syntax bind-to-zero
  (syntax-rules ()
    ;; _ is a shortcut for the macro name
    ;; we tell thet:
    ;; (bind-to-zero id) become (define id 0)
    ((_ id) (define id 0))))
(bind-to-zero x)

(printf "(bind-to-zero x) -> ~a~%" x)

(define-syntax when+
  (syntax-rules ()
    ((_ pred b1 ...)
     (if pred (begin b1 ...) #f))))

(when+ (> x 0)
    (printf "~a > 0\n" x))
(when+ (= x 0)
    (printf "~a = 0\n" x))

(define-syntax for+
  (syntax-rules ()
    ((_ (i from to) b1 ...)
     (let loop((i from))
       (when (< i to)
	  b1 ...
	  (loop (+ 1 i)))))))

(for+ (k 0 10)
  (display k)
  (display #\Space))
(display "\n")