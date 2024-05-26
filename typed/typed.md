# Racket et les types

Il y a plusieurs manières de vérifier le type des arguments d'une fonctions grâce aux nombreux paquets disponibles.

J'ai testé plusieurs solutions:

- `typed/racket`, *Racket* avec des types,
- `swindle`, des fonctions génériques à la *CLOS*,
- `plai-typed`, un drôle de *Scheme* avec un fascinant `type-case`,
- `racket/contract`, permet de définir des contrat, ce qui peut aller au delàde tout ça.

On pourrait utiliser d'autresméthodes, comme avec les classes, interfaces et autres *traits*, mais c'est pas trop mon truc.

## En bref

*Typed/Racket* est le plus complet mais aussi, le plus exigeant. Pour un hobby de retraité, c'est pas la meilleure option. *Swindle* a quelques défauts mais il me convient bien. *Plai Typed* est simple mais limité, il set surtout fait pour un cours intéressant à propos de l'implémentation de langages informatiques. Les *contract* sont un peu lourds mais très efficaces.
