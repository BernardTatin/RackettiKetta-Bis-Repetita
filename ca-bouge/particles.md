# les particules qui bougent et s'entrechoquent

Je suis parti de cet exemple: [Vector of objects](https://github.com/soegaard/sketching/blob/main/sketching-doc/sketching-doc/manual-examples/basics/vectors/vector-of-objects.rkt) qui montre comment utiliser un vecteur d'objet avec *Sketching*. J'ai d'abord rajouté plus de hasard dans le movement des particules puis je les ai libérées de leur peti carré dans lequel elles étaient engoncés. Pour finir j'ai géré les collisions. Et tout cela avec des objets.

Le résultat est plus ou moins conforme à mes attentes mais un peu lent à mon goût. Je vais remplacer ces objets par des fermetures, méthode qui prouvat son efficacité avec des compteurs quant aux performances.

## définition d'une particule

### les champs
Il va être difficile de s'organiser autrement.

- un identificateur unique: `ID`, pour différencier les particules entres elles,
- la position: `x` et `y`,
- la direction: `x-dir` et `y-dir`,
- la vitesse: `speed`,
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

Sans la gestion des collisions, on est à 63/64 fps avec les objets et à 75/76 avec les fermetures.