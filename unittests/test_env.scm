(load "unittest.scm")
(load "eval.scm")

(expect-eq (cons '() '()) (make-frame '() '()))
(expect-eq (cons '(a) '(1)) (make-frame '(a) '(1)))
(expect-eq (cons '(x y) '("a" "hello")) (make-frame '(x y) '("a" "hello")))

(let ((frame (make-frame '(a b) '(1 2))))
  (add-binding-to-frame! 'c 3 frame)
  (expect-eq (make-frame '(c a b) '(3 1 2)) frame))

