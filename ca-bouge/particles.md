# les particules qui bougent et s'entrechoquent

Je suis parti de cet exemple: [Vector of objects](https://github.com/soegaard/sketching/blob/main/sketching-doc/sketching-doc/manual-examples/basics/vectors/vector-of-objects.rkt) qui montre comment utiliser un vecteur d'objet avec *Sketching*. J'ai d'abord rajouté plus de hasard dans le movement des particules puis je les ai libérées de leur petit carré dans lequel elles étaient engoncées. Pour finir j'ai géré les collisions. Et tout cela avec les objets originels de l'exemple.

Le résultat est plus ou moins conforme à mes attentes mais un peu lent à mon goût. Je vais remplacer ces objets par des fermetures, méthode qui prouvat son efficacité avec des compteurs quant aux performances.

## définition d'une particule

### les champs
Il va être difficile de s'organiser autrement.

- un identificateur unique: `ID`, pour différencier les particules entres elles,
- la position: `x` et `y`,
- la direction: `x-dir` et `y-dir`,
- la vitesse: `speed`, ***doit être POSITIVE***!
- la couleur: `bl-color`,
- la forme de l'ellipse: `ex` et `ey`.

Tous ces champs sont publics.

### les méthodes

Il est certainement possible de revoir cette organisation.

- mise à jour de la position: `update`,
- le dessin: `draw`,
- la gestion d'une collision: `on-collision`.

Toutes ces méthodes sont publiques.

### les fonctions utilitaires

Elles s'occupent des collisions.

- détermine la proximité de deux particules, `blob-near?`,
- recherche les collisions possibles: `search-colide`.

## le code commun aux vecteurs et aux fermetures

Ce code est placé dans `particles-lib.rkt`.

## premiers tests

### sans collisions

Sans la gestion des collisions, on est à 63/64 fps avec les objets et à 75/76 avec les fermetures. Lorsqu'on place la procédure de dessin dans la fermeture, on passe à 73/74 fps, mais ça mérite peut-être plus de tests.

Le gain ne semble pas extraordinaire mais l'exercice est enrichissant, continuons.

### avec les collisions

Les objets tournent à 39/40 fps tandis que les fermetures tournent à 69/70 fps. Ici, le gain est très net.

### NOTE IMPORTANTE

Avec les fermetures, le même test se fait parfois en 63 fps au lieu de 70 fps.

## concluons ?

Il y a des nuances dans le traitement des deux cas. En particulier, la fonction `on-collision` (je sais, le nom est nul) est une méthode publique de l'objet alors que c'est une fonction externe à la fermeture.

Pour être honnête, il faudrait faire une version avec une structure. Comme je trouve ça moins drôle que les fermetures, je remet ça à plus tard.

## les avantages de mes fermetures

Tout d'abord, j'ai une animation fluide à 30 fps en plein écran comtre 4 fps.

Ensuite, sans me prendre la tête, j'ai un composant avec des champs en lecture seule. J'aurais pu obtenir ça avec les objets mais certainement avec (beaucoup) plus de code. Et je ne suis pas certain que ça fonctionne facilment avec les structures.
