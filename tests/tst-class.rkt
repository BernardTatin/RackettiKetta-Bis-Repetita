#lang racket

;; le '%' à la fin du nom de la classe est très important
(define my-count%
  (class object%
    ;; pour la construction de l'objet
    (super-new)
    ;; les paramètres du "constructeur" au sens C++
    (init (x0 0)
          (delta0 1))
    ;; des champs
    (field (delta delta0)
           (x x0))
    ;; une méthode privée
    (define/private inc-x
      (lambda()
        (set! x (+ delta x))))
    ;; une méthode publique
    (define/public get-x
      (lambda ()
        (inc-x)
        x))
    ))

(define try-cpt
  (lambda(x0 delta0)
    (let ((cpt (new my-count% (x0 x0) (delta0 delta0))))
      (for ([i (in-range 8)])
        (display (format "new ~a x is ~a~%" i (send cpt get-x)))))))

(try-cpt 10 -2)
(try-cpt 10  2)
(display (format "in-range 5: ~a\n" (in-range 5)))
(display (format "range  0-5: ~a\n" (range 0 5)))
(display (format "square 0-5: ~a\n" (map (lambda(n) (* n n)) (range 0 5))))
(display (format "+0     0-9: ~a\n" (map (lambda(n) (+ n 0)) (range 0 9 2))))