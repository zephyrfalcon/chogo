;; test-macros.scm

(load "chogo.scm")

(pretty-print
 (expand
  '(define-logo-function (jump x)
     (jump-to here)
     (jump-to there)
     (jump-to x))))
