#lang racket

(require "../libs/rolling-cpt.rkt")
(require "../libs/cl-counters.rkt")

(define nloops 25000000)
; (define nloops 25000)
(define chrono-loops 25)

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
    (let ((m (car lst)))
      (letrec ((loop
        (lambda(acc l)
          (cond
            [(null? l) acc]
            [else
              (let ((f (car l)))
                (if (comp? f acc)
                  (loop f (cdr l))
                  (loop acc (cdr l))))]))))
          (loop m (cdr lst))))))
(define amax
  (lambda(lst)
    (aextreme lst >)))
(define amin
  (lambda(lst)
    (aextreme lst <)))



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
            name
            (amin stats) (avg stats chrono-loops) (amax stats))))))


(define x (cl-count  0  1))
(define cl-loop
  (lambda(n)
    (for ((i (in-range 0 n)))
      (x))
    (x)))

(define x+ (cl-count+  0  1))
(define cl-loop+
  (lambda(n)
    (for ((i (in-range 0 n)))
      (x+ 'next))
    (x+ 'get)))

(define cpt (new simple-counter% [cpt 1]))
(define r-loop
    (lambda(n)
    (for ((i (in-range 0 n)))
      (send cpt next))
    (send cpt next)))

;; on my computer:
; cl-count: 39ms
; roll-count: 1122ms

(chrono-stats "cl-count  " cl-loop)
(chrono-stats "roll-count" r-loop)
(chrono-stats "cl-count+ " cl-loop+)