#lang racket

(provide cl-count
         cl-count+)

(define cl-count
    (lambda(from [step 1])
        (let ((cpt from))
            (let ((get-next
                (lambda()
                    (let ((ocpt cpt))
                        (set! cpt (+ cpt step))
                        ocpt))))
            get-next))))

(define cl-count+
  (lambda(from [step 1])
    (let ((cpt from))
      (let ((get-next
             (lambda(option)
               (case option
                 ['get cpt]
                 ['next
                    (set! cpt (+ cpt step))
                    cpt]))))
        get-next))))