(load "util.scm")

;;;; Two main procedures: eval and apply

;;; Except eval the self-evaluating and quotation,
;;; eval other exps should return the self-evaluting or the quote.
;;; In other words, return the representation used in
;;; the interpreted language, not the representation in
;;; the underlying language.
(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
	((let? exp) (eval (let->combination exp) env))
        ((begin? exp) 
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
	((and? exp) (eval-and exp env))
	((or? exp) (eval-or exp env))
        ((application? exp)
         (apply-proc (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type -- EVAL" exp))))

(define (apply-proc procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
           (procedure-body procedure)
           (extend-environment
             (procedure-parameters procedure)
             arguments
             (procedure-environment procedure))))
        (else
         (error
          "Unknown procedure type -- APPLY" procedure))))

;;;; Data-dispatch style of eval
(define (eval-dispatch exp env)
  ;; TODO: finish this function
  'false)


;;;; list-of-values is used in eval an application.
;;;; It's used in evaluated the arguments.
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

;;;; list-of-values-lr-order is used to evaluated the
;;;; argument of an application in LR order.
(define (list-of-values-lr-order exps env)
  (if (no-operands? exps)
      '()
      ;; The LR order is specified at the let variables.
      ;; let can not ensure the evaluation order, but let* can.
      (let* ((first (eval (first-operand exps) env))
	     (rest (list-of-values-lr-order (rest-operands exps) env)))
	(cons first rest))))


;;;; Handling of the special forms
;;;; If expression
(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))


(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))


(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (eval (definition-value exp) env)
                    env)
  'ok)

(define (eval-and exp env)
  (define (eval-and-predicates predicates)
    (cond ((null? predicates) 'true)
	  ((last-predicate? predicates)
	   (eval (first-predicate predicates) env))
	  ((true? (eval (first-predicate predicates) env))
	   (eval-and-predicates (rest-predicate predicates)))
	  (else 'false)))
  (eval-and-predicates (and-predicates exp)))

(define (eval-or exp env)
  (define (eval-or-predicates predicates)
    (cond ((null? predicates) 'false)
	  (else (let ((pred (eval (first-predicate predicates) env)))
		  (cond ((last-predicate? predicates)
			 pred)
			((false? pred)
			 (eval-or-predicates (rest-predicate predicates)))
			(else pred))))))
  (eval-or-predicates (or-predicates exp)))


(define true #t)
(define false #f)
(define (true? a) (not (eq? a 'false)))
(define (false? a) (eq? a 'false))

;;;; Syntax representation
;;;
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
(define (lambda-body exp) (cddr exp))

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
		    ;; a let expression, but right now I've not implement it
		    (list (make-lambda '(__x)
				       (list
					(make-if '__x
						;; application
						(list (cond-recipient first) '__x)
						(expand-clauses rest))))
			  (cond-predicate first)))
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
  (let ((body-without-defines (scan-out-defines body)))
    ;; env is a ptr to the parent env
    (list 'procedure parameters body-without-defines env)))
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

;;;; Environment Representation
;;;
;;; Env is a list of frame, which is just a pair of list.
;;; env => (frame1 frame2 frame3 ....)
;;; frame => ((var1 var2 ...) (value1 value2 ...))

;;; env
(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

;;; frame
(define (make-frame variables values)
  (cons variables values))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (let ((val (car vals)))
	       (if (eq? val '*unassigned*)
		   (error "Uninitialized variable" var)
		   val)))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))


;;;; Primitive procedure representation
(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

;;; primitive procedures in the language
(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
	;; arith ops
        (list '+ +)
	(list '- -)
	(list '* *)
	(list '/ /)
	;; comparison ops
	(list '< (lambda (a b) (if (< a b) 'true 'false)))
	(list '> (lambda (a b) (if (> a b) 'true 'false)))
	(list '= (lambda (a b) (if (= a b) 'true 'false)))
	;; some IO procedures should be added
	))
(define (primitive-procedure-names)
  (map car
       primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (apply-primitive-procedure proc args)
  (apply         ; apply in underlying scheme
   (primitive-implementation proc) args))


;;;; Initial Env
(define (setup-environment)
  (let ((initial-env
	 ;; Putting primitive procedure here also has performance
	 ;; impact.
	 ;; I would move them to anther places later.
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    ;; The true and false binding can be put to the self-evaluating.
    ;; If put here, true and false are searched through variable lookup,
    ;; which is slow compared with self-evaluting.
    ;; Also, I think define primitive types here would make this function
    ;; not so maintainable as defining primitive types in other places.
    ; (define-variable! 'true true initial-env)
    ; (define-variable! 'false false initial-env)
    initial-env))
(define the-global-environment (setup-environment))

