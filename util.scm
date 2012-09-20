(define (concat a b)
  (if (null? a)
      b
      (cons (car a) (concat (cdr a) b))))

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
