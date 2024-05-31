# dessiner

Il y a plusieurs manière d'y arriver avec Racket.

## simple: `sketching`

### require...

```
#lang sketching
```
### doc

### Notes

Ce paquet est un langage à par entière, il défini ses propres fonctionnalités. Par exemple, il n'y a plus `set!` mais un `+=`.

Depuis la console, il faut le lancer avec `Racket`, pas avec `GRacket` (ça marche quand même mais ça ouvre une console supplémentaire inutile).

### description rapide

C'est vrai que c'est simple. L'inconvénient, c'est une boucle infinie qui appelle la méthode `draw` à chaque tour (*cf.* [letters.rkt](letters.rkt)).

### comment ça marche?

***Note:*** voir le code dans [letters.rkt](letters.rkt).

Il *suffit* de définir/ssurcharger quelques fonctions:
 
- `setup`: permet de préparer la fenêtre et la zone de dessin,
- `draw`: c'est ici qu'on fait le dessin qui s'affiche à l'écran,
- `on-size`: déclenché lorsque la fenêtre change de taille.

Il y a aussi de quoi gérer le clavier et la souris.

### conclusion

Ce qui me plaît:
- facile à utiliser,
- se comporte comme une application sous X11, `display` affiche bien les messages sur la console.

Ce qui me plaît moins:
- ça peut bouffer beaucoup de CPU!
