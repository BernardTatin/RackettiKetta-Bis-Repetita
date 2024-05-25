#lang racket

(require racket/match)

;; il existe d'autrs méthodes
;; pour l'instant, on se contente de ça

(define main
  (lambda(args)
         (when (not (null? args))
            (display (format "-> ~a~%" (car args)))
            (main (cdr args)))))


(define (showg v)
  (display (format "Good arg: ~a~%" v)))

(define (showb v)
  (display (format "Unknown arg: ~a~%" v)))

(define (main2 args)
  (match args
    ['() #t]
    [ (cons "1"  tail) (showg 1) (main2 (cdr args))]
    [ (cons "-v" tail) (showg "-v") (main2 (cdr args))]
    [ (cons head tail) (showb head) (main2 (cdr args))]
    ))

(main (vector->list (current-command-line-arguments)))
(main2 (vector->list (current-command-line-arguments)))
