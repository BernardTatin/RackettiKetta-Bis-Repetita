# *Racket* et les classes

## en bref
C'est pas trop compliqué lorsqu'on a pratiqué du *C++* ou du *Java*. Il ne faut pas oublier que le *this* existe, que l'accès aux champs ou méthodes peut être modifié par des *define/private* ou des *define/public*. Pour accéder à une méthode d'un objet, on lui envoie un message `(send object-name method-name parameters ...)`. Voici un exemple:

## un exemple assez complet

Le code se trouve dans le fichier (mal nommé) [test2.ss](test2.ss) où il sera d'ailleurs peut-être plus complet.

```scheme
#lang racket

;; le '%' à la fin du nom de la classe est très important
(define my-count%
  (class object%
    ;; pour la construction de l'objet
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
    ;; initialisation de l'objet
    (super-new)))

(define cpt0 (new my-count% (x0 12)))
(display (format "new x is ~a~%" (send cpt0 get-x)))

(define cpt1 (new my-count% (x0 12) (delta0 -1)))
(display (format "new x is ~a~%" (send cpt1 get-x)))
```
