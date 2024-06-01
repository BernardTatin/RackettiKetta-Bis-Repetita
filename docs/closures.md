# Les fermetures et les compteurs

J'ai fait plusieurs compteurs calquant le fonctionnement de ceux de [rolling-cpt.rkt](../libs/rolling-cpt.rkt) mais avec des fermetures dans [cl-counters.rkt](../libs/cl-counters.rkt].

## Un peu d'*IA* dans mon code
L'un d'entre eux, `cl-count-noset` ne modifie pas de variables, comme les autres. Comme je n'y arrivait pas, j'ai demandé à *Copilot Microsoft* de m'aider. La première version qu'il me donne est la suivante:

```Racket
 (define (cl-count-noset start step)
    (letrec ((next
              (lambda (current)
                (lambda ()
                  (let ((result current))
                    (cons result (next (+ current step))))))))
      (next start)))
```

Le test fourni est le suivant :

```Racket
(define my-counter (cl-count-noset 0 2))
(define counter-state (my-counter)) ; Initialise le compteur
(let loop ((state counter-state))
    (if (< (car state) 10) ; Vérifie si le compteur est inférieur à 10
        (begin
          (display (car state)) ; Affiche la valeur actuelle du compteur
          (newline)
          (loop (cdr state))))) ; Passe au prochain état
```
Le résultat, avec *Racket* comme avec *ChezScheme* est décevant:
```
0
Exception in car: #<procedure> is not a pair
Type (debug) to enter the debugger.
```

Je rappelle *Copilot Microsoft* et là, j'ai le bon `cl-count-noset`, celui que j'ai inclu dans ma `libs`.

### conclusion à relire régulièrement

Il faut donc de la prudence, du test et de la patience lorsqu'on utilise l'IA actuelle. D'un autre côté, cela m'a pris assez peu de temps.