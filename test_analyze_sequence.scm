(load "unittest.scm")
(load "eval-analyze.scm")

(let ((output (analyze-sequence '(1))))
  (expect-eq 1 (output '())))

(let ((output (analyze-sequence '(1 2))))
  (expect-eq 2 (output '())))
