#lang racket

(provide main)


(define task
  (lambda(ID count ms)
    (for ([k (in-range count)])
      (printf "Task ~a - count ~a\n" ID k)
      (sleep ms))
    (printf "Task ~a - END\n" ID)))

(define make-work-place
  (lambda(ID count ms)
    (let ((worker (place self
                         (match (place-channel-get self)
                           [(list ID count ms) (task ID count ms)]))))
      (place-channel-put worker (list ID count ms))
      worker)))

(define create-workers
  (lambda(count)
    (for/list ([ID (in-range count)])
      (make-work-place ID
                       (random 20 25)
                       (exact->inexact
                        (/ (random 600 1600) 1000))))))

(define main
  (lambda ()
    (let ((workers (create-workers 6)))
      (for-each (lambda (e)
                  (place-wait e))
                workers))))
