#lang racket

;;======================================================================
;; all thrads run on the SAME processor !!!!
;;======================================================================

(define end #f)
(define now (current-milliseconds))

(define max-ms 12000.0)

(define task-0
  (lambda(ID)
    (letrec ((ms (exact->inexact (/ (random 4 8) 10)))
             (loop
              (lambda()
                (printf "Task ~a, ~a ~a~%" ID ms (- (current-milliseconds) now))
                (sleep ms)
                (when (not end)
                  (loop)))))
      (loop)
      (printf "End of ~a~%" ID))))

(define task
  (lambda(ID)
    (letrec ((ms (exact->inexact (/ (random 4 8) 10)))
             (loop
              (lambda()
                (let ((dt (- (current-milliseconds) now)))
                  (printf "Task ~a, ~a ~a~%" ID ms dt)
                  (sleep ms)
                  (when (< dt max-ms)
                    (loop))))))
      (loop)
      (printf "End of ~a~%" ID))))

(define run-task
  (lambda(ID lst)
    (let* ((tsk
            (lambda () (task ID)))
           (thd (thread tsk)))
      (cons thd lst))))

(printf "Start!!!!\n")
(define lst-thd (run-task 1 '()))
(set! lst-thd (run-task 2 lst-thd))
(set! lst-thd (run-task 3 lst-thd))
(set! lst-thd (run-task 4 lst-thd))

(sleep 7)
(printf "Stooop!\n")
(set! end #t)
(for-each (lambda(e)
            (when (not (thread-dead? e))
              (displayln "Wait...")
              (thread-wait e)))
          lst-thd)
(printf "Bye!\n")
