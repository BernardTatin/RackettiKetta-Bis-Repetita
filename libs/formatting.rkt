#lang racket

(provide fmt-n)

(define-syntax fmt-n
    (syntax-rules ()
        ((_ x prec mwidth)
            (~a (~r x #:precision '(= prec)) #:width mwidth #:align 'right))))

