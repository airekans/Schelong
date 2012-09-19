(load "unittest.scm")
(load "eval.scm")

(expect-true (if? '(if 1 a b)))
(expect-false (if? 'if))
(expect-false (if? 'and))

(expect-eq 1 (eval-if '(if 3 1 2) '()))
