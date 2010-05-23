;; printing.scm

(define (logo-print x)
  (print (logo-user-repr x)))

;; Return a human-readable representation of a Logo object. Used by PRINT.
(define (logo-user-repr x)
  (cond
   ((eq? x #t)
    "true")
   ((eq? x #f)
    "false")
   ((string? x)
    x)
   ((number? x)
    x)
   ((logo-variable? x)
    ...) ;; XXX need to look this up!
   ((statement? x)
    ...)
   ((list? x)
    ((let ((values (map logo-user-repr x)))
       (string-join values " "))))
   (else (error "Unknown object:" x))))
   

(define (logo-machine-repr x)
  ...)
