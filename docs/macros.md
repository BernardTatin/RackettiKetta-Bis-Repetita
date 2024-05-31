# Les macros ou comment redéfinir la syntaxe...

Le mot clé dans tout ça, c'est `define-syntax`. Voici quelques exemples tirés de [tests/macros.rkt](../tests/macros.rkt).

## simple!
```Racket
(define-syntax bind-to-zero
  (syntax-rules ()
    ((_ id) (define id 0))))
(bind-to-zero x)

(printf "(bind-to-zero x) -> ~a~%" x)
```

Le symbole `_` est un raccorci pour le nom de la macro. La ligne `((_ id) (define id 0))))` se traduit par: turemplace l'expression `(bind-to-zero id)` par l'expression `(define id 0)`. En *C/C++*, on aurait écrit quelque chose comme:

```C
// not tested...
#define bind_to_zero(id) int id 0

bind_to_zero(x);
printf("bind_to_zero(x) -> %d\n", x);
```

## plus compliqué mais plus intéressant

```Racket
(define-syntax when+
  (syntax-rules ()
    ((_ pred b1 ...)
     (if pred (begin b1 ...) #f))))

(define-syntax for+
  (syntax-rules ()
    ((_ (i from to) b1 ...)
     (let loop((i from))
       (when (< i to)
	  b1 ...
	  (loop (+ 1 i)))))))
```

On peut se bousiller les neurones pour trouver un équivalent en C. On peut, mais je ne le ferais pas.

Le `when+`a la même syntaxe que le `when` et s'utilise donc ainsi:
```Racket
(when+ (> x 0)
    (printf "~a > 0\n" x))
(when+ (= x 0)
    (printf "~a = 0\n" x))

```

Tandis que le `for+` va être utiliser de cette manière:

```Racket
(for+ (k 0 10)
  (display k)
  (display #\Space))
(display "\n")
```

Ce `for+` est intéressant car il nous montre comment utiliser une variable, ici nommée `i` dans la macro et `k` lors de l'utilisation de la macro.