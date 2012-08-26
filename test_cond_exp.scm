(load "unittest.scm")
(load "eval.scm")

(let ((output (expand-clauses '())))
  (expect-eq 'false output))

(let ((output (expand-clauses '((else 1)))))
  (expect-eq 1 output))

(let ((output (expand-clauses '((else (first-op)
				      (second-op))))))
  (expect-eq '(begin (first-op)
		     (second-op))
	     output))

(let ((output (expand-clauses '(((+ 1 1) (f 1))))))
  (expect-eq '(if (+ 1 1)
		  (f 1)
		  false)
	     output))

(let ((output (expand-clauses '(((+ 1 1) => fun)
				((* 2 3) (f 2))
				(else "hello")))))
  (expect-eq '(let ((*cond-x* (+ 1 1)))
		(if *cond-x*
		    (fun *cond-x*)
		    (if (* 2 3)
			(f 2)
			"hello")))
	     output))

;;;; Test evaluating the cond expression
(let ((output (eval '(cond (false 1)
			  (2 => (lambda (x) x))
			  (else "hello"))
		    '())))
  (expect-eq 2 output))
