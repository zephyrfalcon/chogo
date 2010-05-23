;; test-logo.scm

(use test)
(load "chogo.scm")

(define (id x) x)

(test-group
 "parsing"

 (with-input-from-string "1 2 (3 4) 5"
   (lambda ()
     (let ((sexps (read-all-sexps (current-input-port))))
       (test '(1 2 (3 4) 5) (id sexps))
       (test 4 (length sexps)))))

 (test '(fd 100 rt 90) (read-sexps-from-string "fd 100 rt 90"))

 (let* ((sexps (read-sexps-from-string "fd 100 rt 90"))
        (state (setup-logo-state)))
   (receive (stmt rest-exprs) (get-next-statement sexps state)
     (test stmt (make-statement '(fd 100)))
     (test '(rt 90) (id rest-exprs))))

 (let* ((sexps (read-sexps-from-string "repeat + :x 1 [rt * :y 6] fd 90"))
        (state (setup-logo-state))
        (ms make-statement))
   (receive (stmt rest-exprs) (get-next-statement sexps state)
     (test stmt
           (ms (list 'repeat (ms '(+ :x 1))
                     ;;(list (ms (list 'rt (ms '(* :y 6))))))))
                     '(rt * :y 6))))
     ;; NOTE: innards of the list [rt * :y 6] are UNPROCESSED at this point!
     ;; they will be once we evaluate the list!
     (test '(fd 90) (id rest-exprs))))
 )

(test-group
 "arity"

 (let* ((state (setup-logo-state))
        (fenv (logo-state-function-env state)))
   (test 1 (logo-function-arity (lookup-function 'fd fenv)))
   (test 2 (logo-function-arity (lookup-function 'repeat fenv))))

 )

(test-group
 "statement: string representation"

 (test "{fd 90}" (sprintf "~s" (make-statement '(fd 90))))
 (test "{* {- 3 4} {+ 5 :x}}"
       (sprintf "~s" (make-statement
                      (list '*
                            (make-statement '(- 3 4))
                            (make-statement '(+ 5 :x))))))
 (test "{repeat 4 ({fd 90})}"
       (sprintf "~s" (make-statement
                      (list 'repeat 4
                            (list (make-statement '(fd 90)))))))
 )

(test-group
 "eval"

 (let* ((state (setup-logo-state))
        (env (logo-state-toplevel-env state))
        (fenv (logo-state-function-env state)))
   (namespace-define! env 'magic 99)

   ;; atoms
   (test 42 (logo-eval 42 env fenv))
   (test 99 (logo-eval ':magic env fenv))

   ;; function calls
   (test 3 (logo-eval (make-statement '(+ 1 2)) env fenv)))

 )

(test-group
 "eval string"

 (let* ((state (setup-logo-state))
        (eval-string (lambda (s)
                       (logo-eval-string
                        s
                        (logo-state-toplevel-env state)
                        (logo-state-function-env state)))))
   
   (test 3 (eval-string "+ 1 2"))
   (test 8 (eval-string "+ * 2 3 - 6 4"))
   
   ))

;; XXX add tests for:
;; TO statement
;; building of the TO construct
;; user-defined functions
;; variable scoping in those functions
