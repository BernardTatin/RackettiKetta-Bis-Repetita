#lang racket

(require racket/class)

(provide simple-counter%
         rolling-cpt%
         rolling-cpt-2d%)

(define i-counter<%>
  (interface ()
    get
    next))

(define simple-counter%
  (class* object% (i-counter<%>)
    (super-new)
    (init-field
     [cpt 0])

    (define/public get
      (lambda()
        cpt))

    (define/public next
      (lambda()
        (set! cpt (+ cpt 1))
        cpt))))

(define rolling-cpt%
  (class* object% (i-counter<%>)
    (super-new)
    (init (min0 0) (max0 0))

    (field (cpt min0) (min min0) (max max0))

    (define/public get
      (lambda()
        cpt))

    (define/public next
      (lambda()
        (let ((newc (+ cpt 1)))
          (if (>= newc max)
              (set! cpt min)
              (set! cpt newc))
          cpt)))))



(define rolling-cpt-2d%
  (class* object% (i-counter<%>)
    (super-new)
    (init (xmin0 0) (xmax0 0)
          (ymin0 0) (ymax0 0))

    (field (x xmin0) (xmin xmin0) (xmax xmax0)
           (y ymin0) (ymin ymin0) (ymax ymax0))


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