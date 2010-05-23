;; logo-1.scm
;; Tiny Logo interpreter written in Chicken Scheme.

;; make things case-insensitive, which is the default for Scheme (and
;; Logo) but not for Chicken
(case-sensitive #f)

(use srfi-1)
(load "namespace")
(load "tools")
(load "builtins")
(load "printing")
;; XXX create a file that loads all auxiliary modules

(define (read-all-sexps port)
  (let loop ((sexps '()))
    (let ((sexp (read port)))
      (if (eof-object? sexp)
          (reverse sexps)
          (loop (cons sexp sexps))))))

(define (read-sexps-from-string s)
  (with-input-from-string s
    (lambda ()
      (read-all-sexps (current-input-port)))))

;;; --- parsing statements ---

;; Get the next statement. Return two values: the statement found, and
;; the sexprs that come after it. If no statement is found, return
;; #f. The statement returned is a #<statement> record with code of
;; the form (f . args), where args may contain sub-statements; if so,
;; they are wrapped in the #<statement> to distinguish them from lists
;; (blocks).
(define (get-next-statement exprs fenv)
  (if (null? exprs)
      #f ;; done
      (let* ((func (car exprs)))
        (cond
         ;; special forms
         ((equal? func 'to)
          (gather-to-statement (cdr exprs)))
         ((equal? func 'make)
          (gather-make-statement (cdr exprs) fenv))

         ;; regular functions
         (else
          (let ((arity (logo-function-arity (lookup-function func fenv))))
            (receive (args rest-exprs)
                (gather-arguments arity (cdr exprs) fenv)
              (values (make-statement (cons (car exprs) args))
                      rest-exprs))))))))

;; dummy type to distinguish sub-statements from lists while parsing
(define-record statement
  code)

(define (gather-to-statement exprs)
  ;; one name (symbol)
  ;; zero or more variables
  ;; a block
  (let ((names (take-while logo-function-name? exprs)))
    (if (= (length names) 1)
        (let* ((rest1 (cdr exprs))
               (var-names (take-while logo-variable? rest1)))
          (let* ((rest2 (drop rest1 (length var-names)))
                 (blocks (take-while list? rest2)))
            (if (>= (length blocks) 1)
                (values (make-statement (list 'to (car names) var-names
                                              (car blocks)))
                        (cdr rest2))
                (error "Invalid TO statement: no block"))))
        (error "Invalid TO statement: function name" names))))

(define (gather-make-statement exprs fenv)
  ;; a variable name
  ;; any normal expression for the value
  (let ((var-name (car exprs)))
    (if (logo-variable? var-name)
        (receive (stmt rest-exprs) (gather-argument (cdr exprs) fenv)
          (values (make-statement (list 'make var-name stmt))
                  rest-exprs))
        (error "Invalid MAKE statement: needs variable name"))))

(define-record-printer statement
  (lambda (stmt port)
    (fprintf port "{~a}" (to-string/remove-parens (statement-code stmt)))))

(define (gather-arguments n exprs fenv)
  (let loop ((gathered '()) (n n) (rest-exprs exprs))
    (if (= n 0)
        (values (reverse gathered) rest-exprs)
        (receive (arg rest) (gather-argument rest-exprs fenv)
          (loop (cons arg gathered) (- n 1) rest)))))

;; Get the next argument (for a function call) from a list of
;; sexps. This can be a list, a number, a variable, etc, but also a
;; nested statement.  Note that lists (blocks) are UNPROCESSED at this
;; point. They will be processed when (and if) they are evaluated.
(define (gather-argument exprs fenv)
  (cond
   ((null? exprs)
    (error "Not enough arguments"))
   ((logo-function-name? (car exprs)) ;; subexpression starts here
    (get-next-statement exprs fenv)) ;; returns 2 values
   (else
    (values (car exprs) (cdr exprs)))))

;;; --- evaluation of Logo expressions ---

(define (logo-special-form? name)
  (member name '(to make)))

(define (logo-variable? exp)
  (and (symbol? exp)
       (string-prefix? ":" (symbol->string exp))))

(define (logo-function-name? exp)
  (and (symbol? exp)
       (not (logo-variable? exp))))

;; returns #t if an expression evaluates to itself.
(define (self-evaluating? exp)
  (or (number? exp)
      (string? exp)
      (list? exp)))

(define (logo-eval exp env fenv)
  (cond
   ((self-evaluating? exp)
    exp)
   ((logo-variable? exp)
    (lookup-variable exp env))
   ((statement? exp) ;; a function call, basically
    (logo-eval-statement exp env fenv))
   (else (error "Could not evaluate:" exp))))

(define (logo-eval-exprs sexps env fenv)
  (let loop ((sexps sexps) (result #f))
    (if (null? sexps)
        result
        (receive (stmt rest-sexps) (get-next-statement sexps fenv)
          (let ((result (logo-eval stmt env fenv)))
            (loop rest-sexps result))))))

(define (logo-eval-statement stmt env fenv)
  (let* ((code (statement-code stmt))
         (func (lookup-function (car code) fenv)) ;; a #<logo-function>
         (args (if (logo-special-form? (car code))
                   (cdr code)
                   (map (lambda (arg) (logo-eval arg env fenv))
                        (cdr code))))) ;; evaluate arguments
    (if (procedure? (logo-function-code func))
        (call-function func args env fenv)
        (let ((newenv (create-namespace env)))
          (bind-vars! newenv (logo-function-parameters func) args)
          (call-function func args newenv fenv)))))

;; interaction with logo-eval-statement is hairy... fix this
(define (call-function logo-func args env fenv)
  (let ((code (logo-function-code logo-func)))
    (if (procedure? code)
        (code env fenv args)
        (logo-eval-exprs code env fenv))))

(define (bind-vars! env names values)
  (for-each (lambda (name value)
              (namespace-define! env name value))
            names values))

(define (logo-eval-string s env fenv)
  (let ((sexps (read-sexps-from-string s)))
    (logo-eval-exprs sexps env fenv)))

;;; --- interpreter state ---

(define-record logo-state
  toplevel-env
  function-env
  )

(define (setup-logo-state)
  (let ((state (make-logo-state (create-namespace #f) (create-namespace #f))))
    (add-builtin-functions state)
    state))

(define (lookup-function name fenv)
  (let* ((result (namespace-get fenv name)))
    (if result
        (second result)
        (error (sprintf "Unknown function: ~s" name)))))

(define (lookup-variable name env)
  (assert (symbol? name))
  (let* ((vname (strip-var-name name))
         (result (namespace-get env vname)))
    (if result
        (second result)
        (error (sprintf "Unknown variable: ~s" vname)))))

;;; --- main program ---

(define (logo-repl)
  (let* ((state (setup-logo-state))
         (env (logo-state-toplevel-env state))
         (fenv (logo-state-function-env state)))
    (let loop ()
      (printf "> ")
      (let* ((s (read-line))
             (sexps (read-sexps-from-string s))
             (result (logo-eval-exprs sexps env fenv)))
        (unless (eq? result (void))  ;; don't display #<unspecified>
          (print result))
        (loop)))))

(define (read-and-eval-file filename)
  (with-input-from-file filename
    (lambda ()
      (let* ((sexps (read-all-sexps (current-input-port)))
             (state (setup-logo-state))
             (env (logo-state-toplevel-env state))
             (fenv (logo-state-function-env state)))
        (logo-eval-exprs sexps env fenv)))))

(define (main args)
  (if (null? args)
      ;; no arguments? enter interactive mode
      (logo-repl)
      ;; otherwise, execute files specified
      (for-each read-and-eval-file args)))
