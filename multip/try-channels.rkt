#lang racket

(module worker racket
  (provide make-worker-place)

  ;; make-worker-place : Any PlaceChannel PlaceChannel -> Place
  (define (make-worker-place id task-in done-out)
    (define worker
      (place self
             (match (place-channel-get self)
               [(list id task-in done-out) (work self id task-in done-out)])))
    (place-channel-put worker (list id task-in done-out))
    worker)

  ;; work : Place Any PlaceChannel PlaceChannel -> Void
  (define (work self id task-in done-out)
    (let loop ()
      (sync (handle-evt task-in
                        (lambda (task)
                          (do-task id task)
                          (place-channel-put done-out task)
                          (loop)))
            (handle-evt self
                        (lambda (shutdown)
                          (printf "place ~s shutting down\n" id)
                          (place-channel-put self 'ok))))))

  ;; do-task : Any Any -> Void
  (define (do-task id task)
    (printf "place ~s got task: ~e\n" id task)
    (sleep (+ 1 (random))))
  )

(require 'worker)

;; Main place sends tasks out using task-out.
;; Workers receive tasks by reading from task-in.
(define-values (task-out task-in) (place-channel))

;; Workers acknowledge completed tasks by sending on done-out.
;; Main place counts completed tasks by reading from done-in.
(define-values (done-out done-in) (place-channel))

;; Create workers.
(define workers
  (for/list ([id (in-range 10)])
    (make-worker-place id task-in done-out)))

;; Put all tasks in the shared task channel.
; (define TASKS #e1e2)
(define TASKS 50)
(for ([n (in-range TASKS)])
  (place-channel-put task-out n))
(printf "finished queueing tasks\n")

;; Wait until all tasks are done before exiting.
(for ([n (in-range TASKS)])
  (void (place-channel-get done-in)))

;; Shut down each worker.
(for ([worker (in-list workers)])
  (place-channel-put worker 'shutdown)
  (void (place-channel-get worker)))