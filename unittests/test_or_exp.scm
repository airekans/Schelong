(load "unittest.scm")
(load "eval.scm")

(expect-true (or? '(or)))
(expect-true (or? '(or 1)))
(expect-true (or? '(or 1 (and 2 3))))
(expect-false (or? '(and)))
(expect-false (or? '1))
(expect-false (or? '(and 1 (or 2 3))))
(expect-false (or? "hello"))

(expect-eq 'false (eval-or '(or) '()))
(expect-eq 1 (eval-or '(or 1) '()))
(expect-eq 1 (eval-or '(or 1 2) '()))
(expect-eq 'true (eval-or '(or true false) '()))
(expect-eq 'false (eval-or '(or false (and false)) '()))
(expect-eq 1 (eval-or '(or false (and 1)) '()))
 
