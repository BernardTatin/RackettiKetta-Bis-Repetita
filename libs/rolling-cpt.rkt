#lang racket

(provide rolling-cpt%)

(define rolling-cpt%
  (class object%
    (super-new)
    (init (cpt0 0) (max0 0))
    (field (cpt cpt0) (max max0))
    (define/public get
      (lambda()
        cpt))
    (define/public next
      (lambda()
        (let ((newc (+ cpt 1)))
          (if (>= newc max)
              (set! cpt 0)
              (set! cpt newc))
          cpt)))))