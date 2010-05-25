;; builtins.scm

(use miscmacros)

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

    (alf 'forward 1 (lambda (env fenv args) ...))
    (alf 'right 1 (lambda (env fenv args) ...))
    (alf 'left 1 (lambda (env fenv args) ...))
    (alf 'say 1
         (lambda (env fenv args)
           (print (car args))))
    (alf 'repeat 2
         (lambda (env fenv args)
           (dotimes (i (car args) (void))
                    (logo-eval-exprs (second args) env fenv))))

    ;; basic arithmetic
    (alf '+ 2 (lambda (env fenv args) (apply + args)))
    (alf '- 2 (lambda (env fenv args) (apply - args)))
    (alf '* 2 (lambda (env fenv args) (apply * args)))

    (alf 'print 1
         (lambda (env fenv args)
           (logo-print (car args))))
    (alf 'show 1
         (lambda (env fenv args)
           (printf "~a~%" (car args))))

    (alf 'interpolate 1
         (lambda (env fenv args)
           (logo-interpolate (car args) env fenv)))

    (alf 'true 0 (lambda (env fenv args) #t))
    (alf 'false 0 (lambda (env fenv args) #f))

    (alf 'eval 1 (lambda (env fenv args)
                   (logo-eval-exprs (car args) env fenv)))

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

    ;; special form: MAKE
    (alf 'make 2
         (lambda (env fenv args)
           (let ((name (strip-var-name (first args)))
                 (value (second args)))
             (namespace-define! env name value))))

    ;; turtle stuff
    (alf 'canvas 2
         (lambda (env fenv args)
           (make-canvas (first args) (second args) 'white)))

    #t))

;;; --- aliases ---

;; first item in the list is the original function name, names after
;; that are the aliases.
(define *function-aliases*
  '((forward fd)
    (right rt)
    (left lt)
    (print pr)
    (interpolate $ intp)))

(define (add-aliases state)
  (for-each
   (lambda (lst)
     (let ((original (car lst))
           (aliases (cdr lst))
           (fenv (logo-state-function-env state)))
       (let ((f (namespace-get fenv original)))
         (for-each (lambda (alias)
                     (namespace-define! fenv alias (second f)))
                   aliases))))
   *function-aliases*))
