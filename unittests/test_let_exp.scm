(load "unittest.scm")
(load "eval.scm")

(expect-true (let? '(let ((x 1)) x)))
(expect-true (let? '(let)))
(expect-false (let? '()))
(expect-false (let? '(cond)))

(expect-eq '() (let-bindings '(let () 1)))
(expect-eq '((x 1)) (let-bindings '(let ((x 1)) x)))
(expect-eq '((x 1) (y 2)) (let-bindings '(let ((x 1) (y 2)) (+ x y))))

(expect-eq '(x) (let-body '(let ((x 1)) x)))
(expect-eq '((+ x y)) (let-body '(let ((x 1) (y 2)) (+ x y))))
(expect-eq '((+ x y) (* x y))
	   (let-body '(let ((x 1) (y 2))
			(+ x y)
			(* x y))))

(let ((output (let->combination '(let ((x 1)
				       (y 2))
				   (+ x y)))))
  (expect-eq '((lambda (x y)
		 (+ x y))
	       1 2)
	     output))

(let ((output (let->combination '(let ((x 1)
				       (y 2))
				   (+ x y)
				   (* x y)))))
  (expect-eq '((lambda (x y)
		 (+ x y)
		 (* x y))
	       1 2)
	     output))

(let ((output (eval '(let ((x 1))
		       x)
		    '())))
  (expect-eq '(thunk 1 ()) output))

(let ((output (eval '(let ((x 1))
		       2
		       x)
		    '())))
  (expect-eq '(thunk 1 ()) output))

(let ((output (actual-value '(let ((x 1))
			       2
			       x)
			    '())))
  (expect-eq 1 output))

(let ((output (eval '(let ((x 1)
			   (y 2))
		       x
		       y)
		    '())))
  (expect-eq '(thunk 2 ()) output))

(let ((output (actual-value '(let ((x 1)
				   (y 2))
			       x
			       y)
			    '())))
  (expect-eq 2 output))
