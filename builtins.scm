;; builtins.scm

(use miscmacros)

(define-record logo-function
  name        ;; name of the function (a symbol)
  arity       ;; number of arguments (an integer >= 0)
  parameters  ;; list of parameter names (as symbols, no leading ':' here)
  code        ;; list of sexps, -OR- a Scheme procedure for builtins
  )

(define *logo-functions* (create-namespace #f))

(define (add-logo-function name arity f)
  (namespace-define! *logo-functions* name
                     (make-logo-function name arity '() f)))

;;; --- macros ---

;; Macro(s) for easy definition of Logo functions. Unhygienic, but we
;; want it that way. ;-) (The idea is that we can still refer to ENV
;; and FENV from within the Logo function's body.)

;; (car exp)   = define-logo-function
;; (cadr exp)  = (name . arglist)
;; (caadr exp) = name
;; (cdadr exp) = arglist
;; (cddr exp)  = body (as a list)
(define-syntax define-logo-function
  (lambda (exp ren cmp)
    (list 'add-logo-function
          (list 'quote (caadr exp))
          (length (cdadr exp))
          (list 'lambda '(env fenv args)
                (list 'apply (cons* 'lambda (cdadr exp) (cddr exp))
                      'args)))))

;;; --- Built-in Logo functions. ---

(define-logo-function (repeat n block)
  (dotimes (i n (void))
           (logo-eval-exprs block env fenv)))

(define-logo-function (+ a b)
  (+ a b))
(define-logo-function (- a b)
  (- a b))
(define-logo-function (* a b)
  (* a b))

(define-logo-function (say x)
  (print x))
(define-logo-function (print x)
  (logo-print x))
(define-logo-function (show x)
  (printf "~a~%" x))

(define-logo-function (interpolate x)
  (logo-interpolate x env fenv))

(define-logo-function (true) #t)
(define-logo-function (false) #f)

(define-logo-function (eval block)
  (logo-eval-exprs block env fenv))

;; special form: TO
(define-logo-function (to name args body)
  (let ((logo-func
         (make-logo-function
          name (length args)
          (map strip-var-name args)
          body)))
    (namespace-define! fenv name logo-func)))

;; special form: MAKE
(define-logo-function (make var-name value)
  (namespace-define! env (strip-var-name var-name) value))


;;; --- aliases ---

;; NOTE: This only works if the original function has already been
;; defined, of course. So we should do this *after* loading turtle
;; stuff and everything.

;; first item in the list is the original function name, names after
;; that are the aliases.
(define *function-aliases*
  '((forward fd)
    (backward back)
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
