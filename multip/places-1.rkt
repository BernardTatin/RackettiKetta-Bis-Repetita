#lang racket

;;======================================================================
;; it works with the following command line:
;; Racket.exe -tm .\places-1.rkt
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
                  (let ((now (current-milliseconds))
                        (N (place-channel-get ch)))
                    (printf "received ~a~%" N)
                    (for ((i (in-range N)))
                      (printf "i -> ~a ~a ms~%" i (- (current-milliseconds) now))
                      (sleep 0.9))
                    (place-channel-put ch "Ok")))))

    (printf "put...\n")
    (place-channel-put p 25)
    (printf "put OK\n")
    (let ((l (place-channel-get p)))
      (printf "the result ~a~%" l))))

; (main)
