#lang racket

(provide cl-count-noset
         with-noset-count
         cl-count
         cl-count+
         cl-count-c-
         cl-count-c+
         cl-rcount
         cl-rcount-2d)


(define (cl-count-noset start step)
  (lambda ()
    (letrec ((next
              (lambda (current)
                (lambda ()
                  (let ((result current))
                    (values result (next (+ current step))))))))
      (next start))))

(define-syntax with-noset-count
  (syntax-rules ()
    ((_ (cpt stop-condition) thunk)
     (let iloop ((next-count (cpt)))
       (call-with-values next-count
                         (lambda (current next)
                           (when (stop-condition current)
                             (thunk current)
                             (iloop next))))))))


(define cl-rcount
  (lambda(vmin vmax [step 1])
    (let ((cpt vmin))
      (let ((get-next
             (lambda()
               (let ((ocpt cpt)
                     (ncpt (+ cpt step)))
                 (if (< ncpt vmax)
                     (set! cpt ncpt)
                     (set! cpt vmin))
                 ocpt))))
        get-next))))

(define cl-rcount-2d
  (lambda(xmin xmax ymin ymax [step 1])
    (let ((x xmin)
          (y ymin))
      (let ((get-next
             (lambda()
               (let ((ox x)
                     (oy y)
                     (nx (+ x step)))
                 (if (< nx xmax)
                     (set! x nx)
                     (begin
                       (set! x xmin)
                       (let ((ny (+ y step)))
                         (if (< ny ymax)
                             (set! y ny)
                             (set! y ymin)))
                       ))
                 (values ox oy)))))
        get-next))))

(define cl-count
  (lambda(from [step 1])
    (let ((cpt from))
      (let ((get-next
             (lambda()
               (let ((ocpt cpt))
                 (set! cpt (+ cpt step))
                 ocpt))))
        get-next))))

(define cl-count+
  (lambda(from [step 1])
    (let ((cpt from))
      (let ((get-next
             (lambda(option)
               (case option
                 ['get cpt]
                 ['next
                  (set! cpt (+ cpt step))
                  cpt]))))
        get-next))))

(define/contract (cl-count-c- from [step 1])
  (-> integer? integer? any/c)
  ; très long (~780 ms au lieu de ~40ms)
  ; (-> integer? integer? (-> integer?))
  (let ((cpt from))
    (let ((get-next
           (lambda()
             (let ((ocpt cpt))
               (set! cpt (+ cpt step))
               ocpt))))
      get-next)))

(define/contract (cl-count-c+ from [step 1])
  ; très long (~780 ms au lieu de ~40ms)
  ; (-> integer? integer? (-> symbol? integer?))
  ;; très rapide (~45ms), cl-count+
  (-> integer? integer? any/c)
  (let ((cpt from))
    (let ((get-next
           (lambda(option)
             (case option
               ['get cpt]
               ['next
                (set! cpt (+ cpt step))
                cpt]))))
      get-next)))
