#lang racket

(require "../libs/rolling-cpt.rkt")
(require "../libs/cl-counters.rkt")

(define nloops 25000000)
; (define nloops 25000)
(define chrono-loops 75)

(define chrono
  (lambda(f)
    (let ((t0 (current-milliseconds)))
      (f nloops)
      (let ((dt (- (current-milliseconds) t0)))
        dt))))

(define show-chrono
  (lambda(name f)
    (let ((dt (chrono f)))
      (printf "~a: ~a\n" name dt))))
(display "======================================================================\n")

(define avg
  (lambda(lst n)
    (let ((s (foldl + 0 lst)))
      (exact->inexact (/ s n)))))

(define aextreme
  (lambda(lst comp?)
    (let ((on-element
           (lambda(e lim)
             (cond
               [(not lim) e]
               [(comp? e lim) e]
               [else lim]
               ))))
      (foldl on-element #f lst))))


(define-syntax amin
  (syntax-rules ()
    ((_ lst) (aextreme lst <))))
(define-syntax amax
  (syntax-rules ()
    ((_ lst) (aextreme lst >))))


(define chrono-stats
  (lambda(name f)
    (letrec ((boucle
              (lambda(k acc)
                (if (= k 0)
                    acc
                    (let ((dt (chrono f)))
                      (boucle (- k 1) (cons dt acc)))))))
      (let ((stats (boucle chrono-loops '())))
        (printf "~a: ~a ~a ~a\n"
                (~a name #:min-width 12)
                (~r (amin stats) #:min-width 8)
                (~r (avg stats chrono-loops) #:precision '(= 2) #:min-width 10)
                (~r (amax stats) #:min-width 8))))))


(define-syntax make-loop
  (syntax-rules ()
    ((_ (name c counter in-loop) out-loop)
     (let ((c counter))
       (define the-loop
         (lambda(n)
           (for ((i (in-range 0 n)))
             in-loop)
           out-loop))
       (chrono-stats name the-loop)))))

(make-loop ("cl-count"
            c (cl-count 0 1) (c))
           (c))

(make-loop ("cl-count+"
            c (cl-count+  0  1) (c 'next))
           (c 'get))

(make-loop ("cl-count-c-"
            c (cl-count-c- 0 1) (c))
           (c))
(make-loop ("cl-count-c++"
            c(cl-count-c+ 0 1) (c 'next))
           (c 'get))

;; presque 30 fois plus lent que les précédents
#|
(make-loop ("simple-c..%"
            c (new simple-counter% [cpt 1]) (send c next))
           (send c next))
|#