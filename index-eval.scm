;;;; index-eval is used to index the scheme buffer
;;;; so that later users can jump to the tag easily

(load "util.scm")


(define index-eval-buffer
  (let (table '())
    (lambda (buf content)
      (define (index-eval-sexps sexps)
	(if (null? sexps)
	    '()
	    (cons (index-eval (car sexps))
		  (index-eval-sexps (cdr sexps)))))
      (let* ((sexps (read-sexps
			 (make-stream (open-input-string content))))
	     (indexed-sexps (index-eval-sexps sexps)))
	(define (make-entry buf content) (cons buf content))
	(define (index-eval-buffer-impl tbl)
	  (cond ((null? tbl) (set! table (cons (make-entry buf indexed-sexps) table)))
		((string=? buf (car (car tbl))) (set-cdr! (car tbl) indexed-sexps))
		(else (index-eval-buffer-impl (cdr tbl)))))
	(index-eval-buffer-impl table)))))

(define (index-eval exp env)
  ())
