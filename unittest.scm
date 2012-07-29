(load "eval.scm")

(define (is-same-type? a b)
  (cond ((and (null? a) (not (null? b))) #f)
	((and (symbol? a) (not (symbol? b))) #f)
	((and (number? a) (not (number? b))) #f)
	((and (string? a) (not (string? b))) #f)
	((and (pair? a) (not (pair? b))) #f)
	(else #t)))

(define (expect-eq actual expected)
  (define (list-eq? a b)
    (cond ((not (is-same-type? a b)) #f)
	  ((null? a) #t)
	  ((symbol? a) (eq? a b))
	  ((number? a) (= a b))
	  ((string? a) (string=? a b))
	  ((pair? a) (and (list-eq? (car a) (car b))
			  (list-eq? (cdr a) (cdr b))))
	  (else (error "Unexpected type in list-eq?"))))
  (if (list-eq? actual expected)
      'ok
      (error "expect-eq:" actual expected)))

(let ((output (expand-clauses '())))
  (expect-eq output 'false))

(let ((output (expand-clauses '((else 1)))))
  (expect-eq output 1))

(let ((output (expand-clauses '((else (first-op)
				      (second-op))))))
  (expect-eq output '(begin (first-op)
			    (second-op))))