# Les fermetures et les compteurs

## c'est quoi
On trouve de bonnes explications ici:
- [Fermeture (informatique)](https://fr.wikipedia.org/wiki/Fermeture_(informatique))
- [An Introduction to Scheme and its Implementation - Understanding lambda](https://www.cs.utexas.edu/ftp/garbage/cs345/schintro-v13/schintro_122.html)

Les langages fonctionels proposent tous - ou presque - la posibilité de créer des fonctions qui retournent une autre fonction, définie à l'intérieur de la précédente. De cette manière, la fonction retournée profite de l'environnement crée autour de sa définition.

## exemple
Voici un exemple simple de compteur:

```racket
(define cl-count
  (lambda(from [step 1])
    (let ((cpt from))
      (let ((get-next
             (lambda()
               (let ((ocpt cpt))
                 (set! cpt (+ cpt step))
                 ocpt))))
        get-next))))
```

L'environnement, ici, est l'ensemble des variables `from`, `step`, et `cpt`. On peut créer autant de compteurs indépendants les uns des autres de cette manière:

```racket
(define cpt+ (cl-count 0  2))
(define cpt- (cl-count 0 -2))

(printf "-> cpt+ ~a cpt- ~a\n" (cpt+) (cpt-))
(printf "-> cpt+ ~a cpt- ~a\n" (cpt+) (cpt-))
(printf "-> cpt+ ~a cpt- ~a\n" (cpt+) (cpt-))
```

On obtient ces trois lignes:

```racket
-> cpt+ 0 cpt- 0
-> cpt+ 2 cpt- -2
-> cpt+ 4 cpt- -4
```

## les compteurs
J'ai fait plusieurs compteurs calquant le fonctionnement de ceux de [rolling-cpt.rkt](../libs/rolling-cpt.rkt) mais avec des fermetures dans [cl-counters.rkt](../libs/cl-counters.rkt].

## les performances

C'est ouachtement plus rapide que les compteurs avec les classes! Sur mon PC, avec les classes, on est de l'ordre de 1100 ms contre 40 pour les fermetures.