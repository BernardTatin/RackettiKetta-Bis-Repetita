#lang racket

;;======================================================================
;; it works with the following command line:
;; Racket.exe -tm .\places-0.rkt
;;======================================================================
(provide main)

(define (any-double? l)
  (for/or ([i (in-list l)])
    (for/or ([i2 (in-list l)])
      (= i2 (* 2 i)))))

(define (run-0)
  (define p
    (place ch
           (define l (place-channel-get ch))
           (define l-double? (any-double? l))
           (place-channel-put ch l-double?)))

  (printf "put...\n")
  (place-channel-put p (list 1 2 4 8))
  (printf "put OK\n")
  (place-channel-get p))

(define (main)
  (let ((p (place ch
                  (let ((l (place-channel-get ch)))
                    (printf "received ~a~%" l)
                    (place-channel-put ch (reverse l))))))

    (printf "put...\n")
    (place-channel-put p (list 1 2 4 8))
    (printf "put OK\n")
    (let ((l (place-channel-get p)))
      (printf "the list: ~a~%" l))))

; (main)