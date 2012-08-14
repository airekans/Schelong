(load "unittest.scm")
(load "eval.scm")

(expect-eq (cons '() '()) (make-frame '() '()))
(expect-eq (cons '(a) '(1)) (make-frame '(a) '(1)))
(expect-eq (cons '(x y) '("a" "hello")) (make-frame '(x y) '("a" "hello")))
