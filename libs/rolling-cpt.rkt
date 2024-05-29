#lang racket

;;======================================================================
(provide simple-counter%
         rolling-cpt%
         rolling-cpt-2d%)

;;======================================================================
(define i-counter<%>
  (interface ()
    get
    next))

;;======================================================================
(define simple-counter%
  (class* object% (i-counter<%>)
    (super-new)
    (init-field [cpt 0] [step 1])

    (define/public get
      (lambda()
        cpt))

    (define/public next
      (lambda()
        (set! cpt (+ cpt step))
        cpt))))

;;======================================================================
(define rolling-cpt%
  (class* object% (i-counter<%>)
    (super-new)
    (init-field (vmin 0) (vmax 0))

    (field (cpt vmin))

    (define/public get
      (lambda()
        cpt))

    (define/public next
      (lambda()
        (let ((newc (+ cpt 1)))
          (if (>= newc vmax)
              (set! cpt vmin)
              (set! cpt newc))
          cpt)))))



(define rolling-cpt-2d%
  (class* object% (i-counter<%>)
    (super-new)
    (init-field (xmin 0) (xmax 0)
                (ymin 0) (ymax 0))

    (field (x xmin)
           (y ymin))


    (define/public get-x
      (lambda()
        x))
    (define/public get-y
      (lambda()
        y))

    (define/public get
      (lambda()
        (values x y)))

    (define/public next
      (lambda()
        (let ((nx (+ x 1)))
          (cond
            [(< x xmax)
             (set! x nx)
             (values nx y)]
            [else
             (let ((ny (+ y 1)))
               (set! x xmin)
               (if (< y ymax)
                   (set! y ny)
                   (set! y ymin))
               (values 0 y))]))))

    ))