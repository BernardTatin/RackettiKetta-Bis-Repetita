#lang racket

(provide cl-count
         cl-count+
         cl-count-c-
         cl-count-c+)

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

(define/contract (cl-count-c- from [step 1])
  (-> integer? integer? any/c)
  ; (-> integer? integer? (-> integer?))
  (let ((cpt from))
    (let ((get-next
           (lambda()
             (let ((ocpt cpt))
               (set! cpt (+ cpt step))
               ocpt))))
      get-next)))

(define/contract (cl-count-c+ from [step 1])
  ; très long (~780 ms)
  ; (-> integer? integer? (-> symbol? integer?))
  ;; très rapide (~45ms), cl-count+
  (-> integer? integer? any/c)
  (let ((cpt from))
    (let ((get-next
           (lambda(option)
             (case option
               ['get cpt]
               ['next
                (set! cpt (+ cpt step))
                cpt]))))
      get-next)))
