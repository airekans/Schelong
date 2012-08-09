(load "unittest.scm")
(load "eval.scm")

(expect-true (and? '(and)))
(expect-true (and? '(and true 1)))
(expect-false (and? 1))
(expect-false (and? 'abc))
(expect-false (and? '(or false)))

