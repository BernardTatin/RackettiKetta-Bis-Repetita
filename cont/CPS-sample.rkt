#lang racket

;; ======================================================================
;; a lot from https://en.wikipedia.org/wiki/Continuation-passing_style
;;
;; ======================================================================
;; double continuation
(define (//& x y ok err)
 (=& y 0.0 (lambda (b)
            (if b
                (err (list "div by zero!" x y))
                (ok (/ x y))))))
;; ======================================================================
;; le dernier argument de f& est la continuation k,
;; il faut appliquer k sur le résultat de f
;; f& prend les arguments (args) de f + la continuation k, i.e. (f& args k)
;; et cps-prim fait simplement (k (f args))
;; NOTE: c'est un exemple de curryfication

(define (cps-prim f)
 (lambda args
  (let ((r (reverse args)))
   ((car r) (apply f (reverse (cdr r)))))))

(define *& (cps-prim *))
(define +& (cps-prim +))
(define -& (cps-prim -))
(define =& (cps-prim =))
(define sqrt& (cps-prim sqrt))

;; ======================================================================
(define (pyth& x y k)
 (*& x x (lambda (x2)
          (*& y y (lambda (y2)
                   (+& x2 y2 (lambda (x2py2)
                              (sqrt& x2py2 k))))))))

(define (factorial& n k) (f-aux& n 1 k))
(define (f-aux& n a k)
 (=& n 0 (lambda (b)
          (if b                    ; unmodified continuation
              (k a)                ; in the recursive call
              (-& n 1 (lambda (nm1)
                       (*& n a (lambda (nta)
                                (f-aux& nm1 nta k)))))))))

;; ======================================================================
;; NOTE: c'est un autre exemple de curryfication
(define (display-lv label)
    (lambda(value)
        (printf "~a ~a~%" label value)))

(define display-x (display-lv "-"))
(define display-error (display-lv "E R R O R"))
(define display-fact (display-lv "factorial"))
(define display-pyth (display-lv "Pythagore"))

(factorial& 10 display-fact)
(pyth& 3 4 display-pyth)
(//& 5 0 display-x display-error)
(//& 5 2 display-x display-error)