(load "eval.scm")
;(load "eval-analyze.scm")

(define schelong-eval eval)
;(define schelong-eval eval-analyze)

(define (driver-loop)
  (define input-prompt ";;; M-Eval input:")
  (define output-prompt ";;; M-Eval value:")
  
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (schelong-eval input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

