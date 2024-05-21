#lang racket

;; le '%' à la fin du nom de la classe est très important
(define base-counter%
  (class object%
    ;; pour la construction de l'objet
    (super-new)

    ;; les paramètres du "constructeur" au sens C++
    (init (x0 0)
          (delta0 1))
    ;; des champs
    (field (delta delta0)
           (x x0))

    ;;
    (define/public next
      (lambda()
        (set! x (+ delta x))
        x))

    ;;
    (define/public get
      (lambda ()
        x))
    ))

(define limited-counter%
    (class base-counter%
        (super-new)
        (init (min-val #f)
              (max-val #f))
          (field (min-x min-val)
                 (max-x max-val))

    (define/private i-next
        (lambda(old-x)
            (let ((new-x (+ old-x (get-field delta this))))
                (cond
                    ((< new-x min-x) min-x)
                    ((> new-x max-x) max-x)
                    (else new-x)))))

    (define/override next
      (lambda()
        (let ((x (i-next (get-field x this))))
        (set-field! x this x)
        x)))
        ))

(define try-cpt
  (lambda(x0 from to delta0)
    (let ((cpt (new limited-counter% (x0 x0)
            (min-val from)
            (max-val to)
            (delta0 delta0))))
      (display "========================================\n")
      (for ([i (in-range 8)])
        (display (format "new ~a x is ~a~%" i (send cpt next)))))))

(try-cpt 6 0 7 -2)
(try-cpt 1 0 6 2)
