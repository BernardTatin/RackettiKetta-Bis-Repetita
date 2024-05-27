#lang racket

;; save the continuation
(define k #f)

;; show the continuation

(define show-k
  (lambda(v)
    (cond
        [k (printf "(k ~a) ~a~%" v (k v))]
        [else (printf "k is #f~%")])))



;; a good 'return' simulation
(define (find-1st-factor factor)
  (let/cc return
    (for ([num (range 2 (+ factor 1))])
      (when (zero? (modulo factor num))
        (return num)))))

(define factors
  (lambda (v)
    (printf "factors of ~a -> ~a~%" v (find-1st-factor v))))

;; a good product
(define with-ret #f)
(define good-p
  (lambda(lst)
    (letrec ((iprod
              (lambda(l acc)
                (cond
                  ((null? l) acc)
                  (else
                   (let ((v (car l)))
                     (let/cc return
                       (if (= 0 v)
                           (begin
                             (set! with-ret #t)
                             (return 0))
                           (iprod (cdr l) (* v acc))))))))))
      (iprod lst 1))))

(define show-p
  (lambda(lst)
    (set! with-ret #f)
    (printf "(* ~a) = ~a (~a)~%" lst (good-p lst) (if with-ret "with-return" "N/A"))))


;; testing
;;
(displayln "======================================================================")
(displayln "Try factors")
(factors 43)
(factors 32)
(factors (* 13 17 23))

;;
(displayln "======================================================================")
(display "Try good-p")
(show-p '(0 1 2 3))
(show-p '(8 1 2 3))
(show-p '(8 1 0 3))
(show-p '(8 1 2 0))
