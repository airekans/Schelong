(load "unittest.scm")
(load "eval-analyze.scm")

(let ((output (analyze-or '(or))))
  (expect-eq 'false (output '())))

(let ((output (analyze-or '(or 1))))
  (expect-eq 1 (output '())))

(let ((output (analyze-or '(or 1 2))))
  (expect-eq 1 (output '())))

(let ((output (analyze-or '(or 1 false))))
  (expect-eq 1 (output '())))

(let ((output (analyze-or '(or false 2))))
  (expect-eq 2 (output '())))

(let ((output (analyze-or '(or false false))))
  (expect-eq 'false (output '())))

(let ((output (analyze-or '(or 1 2 3))))
  (expect-eq 1 (output '())))

(let ((output (analyze-or '(or 1 false 3))))
  (expect-eq 1 (output '())))

(let ((output (analyze-or '(or false 2 3))))
  (expect-eq 2 (output '())))
