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
