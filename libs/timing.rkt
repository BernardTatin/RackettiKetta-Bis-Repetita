#lang racket

(require math/statistics)

(provide with-timing
         with-stats-timing
         stats->string
         avg
         amin
         amax)

(define-syntax with-timing
  (syntax-rules ()
    ((_ () body ...)
     (let ((now (current-inexact-milliseconds)))
       body ...
       (- (current-inexact-milliseconds) now)))))

(define-syntax with-stats-timing
  (syntax-rules ()
    ((_ (loop-counter) body ...)
     (letrec ((iloop
               (lambda(k acc)
                 (if (= 0 k)
                     acc
                     (let ((t (with-timing ()
                                body ...)))
                       (iloop (- k 1) (cons t acc)))))))
       (iloop loop-counter '())))))


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

(define stats->string
  (lambda(stats [min-width 6])
    (format "~a ~a ~a [~a ~a]"
            (~r (amin stats)                #:precision '(= 2) #:min-width min-width)
            (~r (avg stats (length stats))  #:precision '(= 2) #:min-width min-width)
            (~r (amax stats)                #:precision '(= 2) #:min-width min-width)
            (~r (quantile 0.1 < stats)      #:precision '(= 2) #:min-width min-width)
            (~r (quantile 0.9 < stats)      #:precision '(= 2) #:min-width min-width)
            )))