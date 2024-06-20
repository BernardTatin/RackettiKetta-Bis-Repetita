# enfin de vraies particules

Je suis parti d'un exemple d'animation ludique, pédagogique et assez esthétique assez loin des réalités physiques.

## le modèle original.

La différenece fondamentale entre cet exemple et une particule physique, c'est la gestion de la *vitesse*. Dans l'exemple, on a trois variables pour la gérer:
- `x-dir`et `y-dir` qui ne prennet que les valeurs 1 et -1,
- `speed`qui doit être positif ou, éventuellement, nul.

Le déplacement est donné par les formules (les maths sont un peu limitées avec ce markdown):
- $x_{n+1} = x_n+xdir_{n+1}.speed$
- $y_{n+1} = y_n+ydir_{n+1}.speed$

Les $xdir_{n+1}$ et $ydir_{n+1}$ étant modifiés lorsqu'on atteint les bords du rectangle par la série d'équations:
- $xdir_{n+1}= si\ x_n < 0\ ou\ x_n > width\ -xdir_n\ sinon\ xdir_n$
- $ydir_{n+1}= si\ y_n < 0\ ou\ y_n > width\ -ydir_n\ sinon\ ydir_n$

De cette manière, il n'est possible de se déplacer que dans 4 directions uniquement.

## la gestion des collisions

Elle n'est pas très bonne lorsque plus de deux particules s'entrechoquent. Il faudrait créer une liste des particules concernées pour un bon traitement de *l'échange* des vitesses.