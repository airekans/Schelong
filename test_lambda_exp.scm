(load "unittest.scm")
(load "eval.scm")

(expect-true (lambda? '(lambda () 1)))
(expect-true (lambda? '(lambda)))
(expect-false (lambda? 'lambda))
(expect-false (lambda? "lambda"))

(expect-eq (cons 'lambda (cons '(a) 'a)) (make-lambda '(a) 'a))
