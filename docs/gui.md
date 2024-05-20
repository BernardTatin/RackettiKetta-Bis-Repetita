# Des fenêtres et des dessins

## dessiner

Pour dessiner (et aussi écrire), il faut un composant *Canva%* comme dans cet exemple (*cf.* sample-app*.ss):

```scheme
(define my-canvas%
  (class canvas% ; The base class is canvas%
    ;; -------------------------------------------------------
    ;; a set of fields
    (field (current-msg "Nothing to do!")
           (count-keys 0)
           (count-mouse 0)
           (k-last " "))


    (define reset-msg
      (lambda [new-msg]
        (set! current-msg new-msg)
        (send this on-paint)))

    ;; -------------------------------------------------------
    ;; Define overriding method to handle keyboard events
    (define aux-eventspace (make-eventspace))

    (define/private the-key-code
        (lambda(k-event)
            (let ((k (send k-event get-key-code)))
                (case k
                    ((press release) k-last)
                    (else
                        (set! k-last k)
                        k)))))

    (define/override (on-char ke)
      (parameterize ((current-eventspace aux-eventspace))
        ;; work is send to another thread
        (queue-callback
         (lambda ()
           (set! count-keys (+ count-keys 1))
           (reset-msg (format "Canvas keyboard ~a - ~a" count-keys
                (the-key-code ke)))))))
    ;; -------------------------------------------------------
    [ define/private (my-paint-callback myself dc)
       (send dc set-scale 1 1)
       (send dc erase)
       (send dc set-text-foreground "#444444")
       (send dc draw-text current-msg x0 y0)
       (define-values (w h d a) (send dc get-text-extent current-msg))
       (send dc draw-line x0 (+ y0 h) (+ x0 w) (+ y0 h))
       (send dc draw-text (format "~a x ~a" w h) x0 (+ y0 h 2))
       ]
    ;; -------------------------------------------------------
    ; Define overriding method to handle mouse events
    (define/override (on-event event)
      (set! count-mouse (+ count-mouse 1))
      (reset-msg (format "Canvas event ~a" count-mouse)))
    ;; -------------------------------------------------------
    ; Call the superclass init, passing on all init args
    (super-new (paint-callback (lambda (c dc) (my-paint-callback c dc))))))
```

## des détails

***NOTE***: à revoir et se souvenir qu'on ne traite que du *Canvas%* pour le moment.

Cet exemple montre comment gérer les évenements *paint* (`my-paint-callback`), du clavier (`on-char`) et ***tous les autres*** (`on-event`). De la même manière, les évennements souris et autres semblent bien traités dans le même callback.

 ## les évennements clavier

 C'est simple, on surcharge la méthode `on-char` qui reçoit en paramètres l'évennement. Cet évennement contient tout ce qu'on peut connaître comme le code mais aussi les touches de type *Alt*, *Ctrl*, ...

 La fonction `the-key-code` tente de résoudre (mal) ce problème.