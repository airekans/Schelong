(load "util.scm")

;;;; Syntax representation

(define true #t)
(define false #f)
(define (true? a) (not (eq? a 'false)))
(define (false? a) (eq? a 'false))

;;; NOTE: Eval expressions other than self-evaluating should returns
;;; self-evaluating, not a internal represented value.
(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
	((or (eq? 'true exp) (eq? 'false exp)) true)
        (else false)))

(define (variable? exp) (symbol? exp))

;;; Quotation
(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))
(define (make-quoted exp)
  (list 'quote exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

;;; Assignment
(define (assignment? exp)
  (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

(define (make-assignment var val)
  (list 'set! var val))

;;; Definition
(define (definition? exp)
  (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)   ; formal parameters
                   (cddr exp)))) ; body

;;; Lambda expression
;;  lambda has the form of (lambda (x y) (expr))
(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (scan-out-defines (cddr exp)))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

;;; let expression
(define (let? exp) (tagged-list? exp 'let))
(define (let-bindings exp) (cadr exp))
(define (let-body exp) (cddr exp))

(define (let->combination exp)
  (define (let-variables bindings)
    (map car bindings))
  (define (let-variable-values bindings)
    (map cadr bindings))
  (cons (make-lambda (let-variables (let-bindings exp))
		     (let-body exp))
	(let-variable-values (let-bindings exp))))

(define (make-let-binding var val)
  (list var val))
(define (make-let bindings exps)
  (cons 'let (cons bindings exps)))

;;; If expression
(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))
(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

;;; begin expression
(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))

;;; Application
(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

;;; cond expression
(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond-=>? clause) (eq? '=> (cadr clause)))
(define (cond-recipient clause) (caddr clause))
(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false                          ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF"
                       clauses))
	    (if (cond-=>? first)
		(if (null? (caddr first))
		    (error "=> clause doesn't have the recipient -- COND->IF")
		    (make-let (list (make-let-binding '*cond-x*
						      (cond-predicate first)))
			      (list
			       (make-if '*cond-x*
					;; application
					(list (cond-recipient first) '*cond-x*)
					(expand-clauses rest)))))
		(make-if (cond-predicate first)
			 (sequence->exp (cond-actions first))
			 (expand-clauses rest)))))))

;;; and expression
(define (and? exp)
  (tagged-list? exp 'and))

(define (and-predicates exp)
  (cdr exp))
(define (last-predicate? predicates)
  (null? (cdr predicates)))
(define (first-predicate predicates)
  (car predicates))
(define (rest-predicate predicates)
  (cdr predicates))

;;; or expression
(define (or? exp)
  (tagged-list? exp 'or))
(define (or-predicates exp)
  (cdr exp))

;;; procedure representation, the result of eval a lambda
(define (make-procedure parameters body env)
  ;; env is a ptr to the parent env
  (list 'procedure parameters body env))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

;; procedure used to process internal definitions
(define (defines-exps->let defines exps)
  (let ((bindings (map (lambda (d)
			 (list (definition-variable d)
			       '*unassigned*))
		       defines))
	(assignments (map (lambda (d)
			    (make-assignment (definition-variable d)
					     (definition-value d)))
			  defines)))
    (make-let bindings (concat assignments exps))))

(define (scan-out-defines body)
  (if (null? body)
      '()
      (let ((defines (filter (lambda (exp)
			       (definition? exp))
			     body))
	    (exps (filter (lambda (exp)
			    (not (definition? exp)))
			  body)))
	(if (null? defines)
	    body
	    (list (defines-exps->let defines exps))))))

;;;; Primitive procedure representation
(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

;;;; Debugging support
(define (break? exp)
  (tagged-list? exp 'break))
(define (debug? exp)
  (tagged-list? exp 'debug-on-entry))

