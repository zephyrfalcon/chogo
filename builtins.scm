;; builtins.scm

(define-record logo-function
  name        ;; name of the function (a symbol)
  arity       ;; number of arguments (an integer >= 0)
  parameters  ;; list of parameter names (as symbols, no leading ':' here)
  code        ;; list of sexps, -OR- a Scheme procedure for builtins
  )

(define (add-logo-function fenv name arity f)
  (namespace-define! fenv name
                     (make-logo-function name arity '() f)))

(define (add-builtin-functions state)
  (let* ((env (logo-state-toplevel-env state))
         (fenv (logo-state-function-env state))
         (alf (lambda (name arity f) ;-)
                (add-logo-function fenv name arity f))))

    (alf 'fd 1 (lambda (env fenv args) ...))
    (alf 'rt 1 (lambda (env fenv args) ...))
    (alf 'say 1
         (lambda (env fenv args)
           (print (car args))))
    (alf 'repeat 2 (lambda (env fenv args) ...))
    (alf '+ 2 (lambda (env fenv args) (apply + args)))
    (alf '- 2 (lambda (env fenv args) (apply - args)))
    (alf '* 2 (lambda (env fenv args) (apply * args)))

    (alf 'print 1
         (lambda (env fenv args)
           (logo-print (car args))))

    (alf 'true 0 (lambda (env fenv args) #t))
    (alf 'false 0 (lambda (env fenv args) #f))

    ;; special form: TO
    (alf 'to 3
         (lambda (env fenv args)
           (let ((name (first args))
                 (var-names (second args))
                 (block (third args)))
             (let ((logo-func (make-logo-function
                               name (length var-names)
                               (map strip-var-name var-names)
                               block)))
               (namespace-define! fenv name logo-func)))))

    #t))
