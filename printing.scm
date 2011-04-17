;; printing.scm

;; FIXME: strings in lists need to be printed without quotes
(define (logo-print x)
  (if (list? x)
      (print (to-string/remove-parens (map logo-human-repr x)))
      (printf "~a~%" x)))

;; TODO: human-readable vs machine-readable representations

(define (logo-human-repr x)
  (cond ((eq? x #t)
         "true")
        ((eq? x #f)
         "false")
        ((string? x)
         x)
        (else
         (sprintf "~a" x))))

(define (logo-interpolate x env fenv)
  (cond ((list? x)
         (map (cut logo-interpolate <> env fenv) x))
        ((logo-variable? x)
         (lookup-variable x env))
        (else x)))
