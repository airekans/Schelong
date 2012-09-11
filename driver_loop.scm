(load "eval.scm")
;(load "eval-analyze.scm")

;;; The schelong-eval must be actual-value instead of eval,
;;; because eval merely return the object if it's a thunk.
;;; And it's possible to return a thunk in the top level
;;; read-write loop.
(define schelong-eval actual-value)
;(define schelong-eval eval-analyze)

(define input-prompt ";;; L-Eval input:")
(define output-prompt ";;; L-Eval value:")
(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (schelong-eval input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))
(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

;;;; user-print is used to print the evaluated object in user-readable format.
;;;; This is used to avoid the complex output of the compound object.
(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))

