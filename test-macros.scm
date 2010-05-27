;; test-macros.scm
;; Not a real test case... merely prints out some macroexpansions.

(load "chogo.scm") ;; load everything

(pretty-print
 (expand
  '(define-logo-function (jump x)
     (jump-to here)
     (jump-to there)
     (jump-to x))))
