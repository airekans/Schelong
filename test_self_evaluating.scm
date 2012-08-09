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
