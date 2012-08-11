(load "unittest.scm")
(load "eval.scm")

(expect-true (and? '(and)))
(expect-true (and? '(and true 1)))
(expect-false (and? 1))
(expect-false (and? 'abc))
(expect-false (and? '(or false)))

;;; assume that eval self-evaluating exp is OK
;;; test the eval-and function
(expect-true (eval-and '(and) '()))
(expect-eq 1 (eval-and '(and 1) '()))
(expect-eq 2 (eval-and '(and 1 2) '()))
(expect-eq 3 (eval-and '(and 1 (and 2 3)) '()))
(expect-eq 'false (eval-and '(and false) '()))
(expect-eq 'false (eval-and '(and 1 false 2) '()))
;;; currently there is no way to let eval return a false.
