(load "eval.scm")

(let ((output (expand-clauses '())))
  (expect-eq output 'false))

(let ((output (expand-clauses '((else 1)))))
  (expect-eq output 1))

(let ((output (expand-clauses '((else (first-op)
				      (second-op))))))
  (expect-eq output '(begin (first-op)
			    (second-op))))
