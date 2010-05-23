;; printing.scm

(define (logo-print x)
  (if (list? x)
      (print (to-string/remove-parens x))
      (printf "~a~%" x)))

;; TODO: human-readable vs machine-readable representations

