# *Racket* et les classes

## en bref
C'est pas trop compliqué lorsqu'on a pratiqué du *C++* ou du *Java*. Il ne faut pas oublier que le *this* existe, que l'accès aux champs ou méthodes peut être modifié par des *define/private* ou des *define/public*. Pour accéder à une méthode d'un objet, on lui envoie un message `(send object-name method-name parameters ...)`.

Un bon exemple se trouve dans [libs/rolling-cpt.rkt](libs/rolling-cpt.rkt).

## des détails

### les classes
On défini une classe avec `class`ou `class*` (*cf.* l'exemple plus bas):
- `class` pour un héritage simple *sans* interface,
- `class*` pour un héritage *avec* interface, y compris la liste vide `'()`.

Les particularités des champs:
- `init`, pour les valeurs passées au constructeur,
- `init-field`, comme `init`mais en plus crée un champs du même nom,
- `field`, des champs.

Avec un peu de pratique, en général il vaut mieux utiliser `init-field` pour éviter du code pas beau

### les interfaces
Voici un exemple (*cf.* [libs/rolling-cpt.rkt](libs/rolling-cpt.rkt)):

```racket
(define i-counter<%>
  (interface ()
    get
    next))

(define simple-counter%
  (class* object% (i-counter<%>)
    (super-new)
    ...
  ))
```

- ***Note 1:*** : Pour utiliser les interfaces, il est bien précisé dans la documentation qu'il faut utiliser `class*` et non `class` (*cf.* [Classes and Objects](https://docs.racket-lang.org/guide/classes.html#%28part._.Interfaces%29) sur le site officiel).
- ***Note 2:*** : les méthodes de la classe ***doivent être*** publiques - c'est bête, mais faut pas oublier.

## Traits, mixins et contracts...
On peut agir d'étrange façon sur les classes pour les contraindre, les transformer, les dériver... C'est bien et beau, mais pour le moment on se contentera des interfaces et classes.

## un exemple assez complet

Dans [libs/rolling-cpt.rkt](libs/rolling-cpt.rkt), on trouvera des exemples plus complets.

```racket
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
