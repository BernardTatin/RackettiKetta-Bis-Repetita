# `typed/racket`, bien mais un peu lourd

## il y a longtemps, déjà...

Y a des jours où le gris du ciel, la fraîcheur humide d'un mauvais printemps nous démoralisent. Ces jours là, les erreurs de type bêtes et diaboliquement inextricables qui nous tombent sur la tronche peuvent pousser à la dépression. Difficile de donner un exemple simple et court de ce qui m'est arrivé à l'époque - je n'ai plus le code. C'est à ce moment que j'ai cherché comment typer mes fonctions. En fait, à l'époque j'ai opté pour *[Swindle](https://docs.racket-lang.org/swindle/index.html)* qui offre des fonctions génériques à la *Lisp*. *Swindle* m'a sorti d'affaire mais m'a posé quelques soucis d'organinsation de code et de performances.  J'aurais bien aimé avoir le `typed/racket` d'aujourd'hui.

On pourrait arguer qu'avec la programmation objet, on peut se sortir de ces situations embarrassantes. Mais je ne voulais ***pas*** de ce paradigme. Et je tiens toujours à limiter mes contacts avec ce truc.

C'était *autrefois*, les ordinateurs d'aujourd'hui ont des performances qui permettent d'oublier beaucoup de détails qui nous irritaient à ce moment là ***et*** `typed/racket`s'est bien stabilisé et complété, surtout au niveau de la documentation.

## ce qui m'agace

La définition du type d'une fonction n'est pas régulière. Lorsqu'on défini une fonction *top-level*, on donne le type ***avant*** la fonction. Lorsqu'on utilise `letrec`, on donne le type ***après*** le nom, juste avant le `lambda`. Voici un exemple ([try.rkt](try.rkt)):

```Racket
(: fact : Integer -> Integer)
(define (fact n)
  (letrec ((ifact : (Integer Integer -> Integer)
    (lambda (k acc)
                    (if (<= k 0)
                        acc
                        (ifact (- k 1) (* k acc))))))
    (ifact n 1)))
```

De manière plus générale, plutôt que `let`, `let*` et `letrec`, je trouve que `define` serait tout aussi efficace comme ici (ça marche pas):

```Racket
(: fact : Integer -> Integer)
(define (fact n)
  (: ifact : Integer Integer -> Integer)
  (define ifact(k acc)
          (if (<= k 0)
            acc
            (ifact (- k 1) (* k acc))))
    (ifact n 1))
```

## pourquoi je ne l'utiliserais pas

Je suis là pour le fun et le dérouillage de mes vieillissants neurones, pas pour une performance permanente style prise de tête. Ceci dit, por ceux qui ne maîtrisent pas forcément bien les types, c'est un excellent apprentissage.