#lang plai-typed

#|
    c'est ... sportif

    - beaucoup de fonctions ne sont pas accessibles directement,
    - le corps de la fonctino ne supporte qu'une expression,
        i.e. pas de begin caché, il est obligatoire
|#

;; define: bad syntax
(define (show-string [mon-t : string]) : void
  (begin
    (display "String ")
    (display mon-t)
    (display "\n")
    ))

(define (show-int [mon-t : number]) : void
  (begin
    (display "Integer ")
    (display mon-t)
    (display "\n")))
;;number->string is unbound
; (define (show-int [mon-t : number]) : void
;     (display (string-append "Integer "
;         (string-append (number->string mon-t) "\n")))
;     )

(define-type MisspelledAnimal
  [caml (humps : number)]
  [yacc (height : number)])

(define (good? [ma : MisspelledAnimal]) : boolean
  (type-case MisspelledAnimal ma
    [caml (humps) (>= humps 2)]
    [yacc (height) (> height 2.1)]))
(define ma1 (caml 2))
(define ma2 (yacc 1.9))

(test (good? ma1) #t)
(test (good? ma2) #f)

;;======================================================================
(define-type TArg
  [int (value : number)]
  [str (text : string)]
  )


(define (show-arg [arg : TArg]) : void
  (type-case TArg arg
    [int (v) (show-int v)]
    [str (t) (show-string t)]
    ))

(define a1 (str "Coucou"))
(define a2 (int 1957))

(show-arg a1)
(show-arg a2)