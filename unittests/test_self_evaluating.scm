(load "unittest.scm")
(load "eval.scm")


(let ((output (self-evaluating? 1)))
  (expect-eq true output))

(let ((output (self-evaluating? "hello")))
  (expect-eq true output))

(let ((output (self-evaluating? 'abc)))
  (expect-eq false output))

(let ((output (self-evaluating? '())))
  (expect-eq false output))

(let ((output (self-evaluating? '(a b c))))
  (expect-eq false output))

;;;; Test eval self-evaluating
(expect-eq 1 (eval 1 '()))
(expect-eq "a" (eval "a" '()))
(expect-eq "abc" (eval "abc" '()))

