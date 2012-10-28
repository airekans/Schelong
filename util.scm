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

(define (make-line-col-index line col)
  (cons line col))
(define (line-index lci) (car lci))
(define (col-index lci) (cdr lci))
(define (line-index-1+ lci) (cons (1+ (car lci)) (cdr lci)))
(define (col-index-1+ lci) (cons (car lci) (1+ (cdr lci))))

(define (make-indexed-token tok beg-cli end-cli)
  (list tok beg-cli end-cli))
(define (token itok) (car itok))
(define (beg-cli itok) (cadr itok))
(define (end-cli itok) (caddr itok))

(define delimiters (string->char-set " ()"))

(define (read-sexp)
  (let ((buffer "")
	(line-num 0)
	(col-num 0))
    (read-line (string->char-set "() "))))

(define (read-new-self-eval beg)
  (define (read-self-eval buffer end)
    (if (char-set-member? delimiters (peek-char))
	(make-indexed-token (read (open-input-string
				   (reverse-string (list->string buffer))))
			    beg end)
	(read-self-eval (cons (read-char) buffer) (col-index-1+ end))))
  (read-self-eval '() beg))

