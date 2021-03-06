;; tools.scm

(define (to-string/remove-parens lst)
  (let ((s (sprintf "~a" lst)))
    (if (list? lst)
        (substring s 1 (- (string-length s) 1))
        s)))

(define (strip-var-name var-name)
  (let ((s (symbol->string var-name)))
    (if (string-prefix? ":" s)
        (string->symbol (substring s 1))
        s)))

;; simple point accessors (where a point is a list of 2 numbers ;-)
(define x-of first)
(define y-of second)
