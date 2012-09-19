(load "unittest.scm")
(load "eval.scm")

(let ((output (variable? 'a)))
  (expect-true output))

(let ((output (variable? (car '(a 1 2)))))
  (expect-true output))

(let ((output (variable? 1)))
  (expect-false output))

(let ((output (variable? "abc")))
  (expect-false output))

(let ((output (variable? '(1 2 3))))
  (expect-false output))

(let ((output (variable? '())))
  (expect-false output))
