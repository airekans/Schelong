(load "util.scm")
(load "syntax.scm")
(load "env.scm")

;;;; Two main procedures: eval and apply

;;; Except eval the self-evaluating and quotation,
;;; eval other exps should return the self-evaluting or the quote.
;;; In other words, return the representation used in
;;; the interpreted language, not the representation in
;;; the underlying language.
(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
	((break? exp) (eval-break exp env))
	((debug? exp) (eval-debug exp env))
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

;;;; Debugging procedure definitions
(define (eval-break exp env)
  (define input-prompt ";;; Debug-Eval input:")
  (define output-prompt ";;; Debug-Eval value:")
  (define (continue? exp) (tagged-list? exp 'continue))
  (define (debug-loop)
    (prompt-for-input input-prompt)
    (let ((input (read)))
      (if (continue? input)
	  'true
	  (begin (announce-output output-prompt)
		 (user-print (eval input env))
		 (debug-loop)))))
  (debug-loop))

(define (insert-break proc)
  (let ((new-body (cons '(break)
			(procedure-body proc))))
    (make-procedure (procedure-parameters proc)
		    new-body
		    (procedure-environment proc))))

(define (eval-debug exp env)
  (let ((var (debug-procedure exp)))
    (if (variable? var)
	(let ((val (eval var env)))
	  (if (compound-procedure? val)
	      (begin
		(set-variable-value! var (insert-break val) env)
		'ok)
	      (error "Not lambda -- DEBUG-ON-ENTRY" exp)))
	(error "Not function name -- DEBUG-ON-ENTRY" exp))))

(define (debug-eval exp env)
  (announce-output ";;; Debug Exp:")
  (user-print exp)
  (let ((op (eval-break '(break) env)))
    (cond ((continue? op)
	   (begin (set! eval eval-exp)
		  (eval exp env)))
	  ((next? op)
	   (begin (set! eval eval-exp)
		  (let ((val (eval exp env)))
		    (set! eval debug-eval)
		    val)))
	  ((step-in? op) (eval-exp exp env))
	  (else (error "Unknown op -- DEBUG-EVAL" exp)))))
