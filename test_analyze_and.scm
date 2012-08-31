(load "unittest.scm")
(load "eval-analyze.scm")

(let ((output (analyze-and '(and))))
  (expect-eq 'true (output '())))

(let ((output (analyze-and '(and 1))))
  (expect-eq 1 (output '())))

(let ((output (analyze-and '(and 1 2))))
  (expect-eq 2 (output '())))

(let ((output (analyze-and '(and false 2))))
  (expect-eq 'false (output '())))

(let ((output (analyze-and '(and 1 false))))
  (expect-eq 'false (output '())))

(let ((output (analyze-and '(and 1 2 3))))
  (expect-eq 3 (output '())))

(let ((output (analyze-and '(and 1 false 3))))
  (expect-eq 'false (output '())))
