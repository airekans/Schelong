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

;;; primitive procedures in the language
(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? (lambda (l) (if (null? l) 'true 'false)))
	;; arith ops
        (list '+ +)
	(list '- -)
	(list '* *)
	(list '/ /)
	(list '% remainder)
	;; comparison ops
	(list '< (lambda (a b) (if (< a b) 'true 'false)))
	(list '> (lambda (a b) (if (> a b) 'true 'false)))
	(list '= (lambda (a b) (if (= a b) 'true 'false)))
	;; some IO procedures should be added
	(list 'display display)
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

