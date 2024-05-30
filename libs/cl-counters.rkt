#lang racket

(provide cl-count)

(define cl-count
    (lambda(from [step 1])
        (let ((cpt from))
            (let ((get-next
                (lambda()
                    (let ((ocpt cpt))
                        (set! cpt (+ cpt step))
                        ocpt))))
            get-next))))

(define x+ (cl-count  0  1))
(define x- (cl-count 14 -1))
(for ((i (in-range 0 15)))
    (printf "i: ~a, cpt: ~a - ~a~%" i (x+) (x-)))

