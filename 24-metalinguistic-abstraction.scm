;; You could call this simply "eval", but depending on the Scheme
;; implementation you use (I like Guile), hilarity may ensue.
(define (sicp-eval exp env)
  (cond ((self-evaluating? exp) exp)
	((variable? exp) (lookup-variable-value exp env))
	((quoted? exp) (text-of-quotation exp))
	((assignment? exp) (eval-assignment exp env))
	((definiton? exp) (eval-definition exp env))
	((if? exp) (eval-if exp env))
	((lambda? exp) (make-procedure (lambda-parameters exp)
				       (lambda-body exp)
				       env))
	((begin? exp)
	 (eval-sequence (begin-actions exp) env))
	((cond? exp) (sicp-eval (cond->if exp) env))
	((let? exp) (sicp-eval (let->combination exp) env))
	((let*? exp) (sicp-eval (let*->nested-lets exp) env))
	((and? exp) (eval-and exp env))
	((or? exp) (eval-or exp env))
	((application? exp)
	 (sicp-apply (sicp-eval (operator exp) env)
		     (list-of-values (operands exp) env)))
	(else
	 (error "Unknown expression type: SICP-EVAL" exp))))

(define (sicp-apply procedure arguments)
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
	 (error "Unknown procedure type: SICP-APPLY" procedure))))

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (sicp-eval (first-operand exps) env)
	    (list-of-values (rest-operands exps) env))))

(define (eval-if exp env)
  (if (true? (sicp-eval (if-predicate exp) env))
      (sicp-eval (if-consequent exp) env)
      (sicp-eval (if-alternative exp) env)))

(define (eval-sequence exps env)
  (cond ((last-exp? exps)
	 (sicp-eval (first-exp exps) env))
	(else
	 (sicp-eval (first-exp exps) env)
	 (eval-sequence (rest-exps exps) env))))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
		       (sicp-eval (assignment-value exp) env)
		       env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
    (sicp-eval (definition-value exp) env)
    env)
  'ok)

;; Exercise 4.1
(define (list-of-values-front-first exps env)
  (if (no-operands? exps)
      '()
      (let ((evalled (sicp-eval (first-operand exps) env)))
	(cons evalled
	      (list-of-values (rest-operands exps) env)))))

(define (list-of-values-back-first exps env)
  (if (no-operands? exps)
      '()
      (let ((evalled (list-of-values (rest-operands exps) env)))
	(cons (sicp-eval (first-operand exps) env)
	      evalled))))


;; Representing Expressions
(define (self-evaluating? exp)
  (cond ((number? exp) #t)
	((string? exp) #t)
	(else #f)))

(define (variable? exp)
  (symbol? exp))

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp)
  (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      #f))

(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp)
  (cadr exp))

(define (assignment-value exp)
  (caddr exp))

(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
		   (cddr exp))))

(define (lambda? exp)
  (tagged-list? exp 'lambda))

(define (lambda-parameters exp)
  (cadr exp))

(define (lambda-body exp)
  (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (if? exp)
  (tagged-list? exp 'if))

(define (if-predicate exp)
  (cadr exp))

(define (if-consequent exp)
  (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (begin? exp)
  (tagged-list? exp 'begin))

(define (begin-actions exp)
  (cdr exp))

(define (last-exp? seq)
  (null? (cdr seq)))

(define (first-exp seq)
  (car seq))

(define (rest-exps seq)
  (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
	((last-exp? seq) (first-exp seq))
	(else (make-begin seq))))

(define (make-begin seq)
  (cons 'begin seq))

(define (application? exp)
  (pair? exp))

(define (operator exp)
  (car exp))

(define (operands exp)
  (cdr exp))

(define (no-operands? ops)
  (null? ops))

(define (first-operand ops)
  (car ops))

(define (rest-operands ops)
  (cdr ops))


;; Derived expressions
(define (cond? exp)
  (tagged-list? exp 'cond))

(define (cond-clauses exp)
  (cdr exp))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-predicate clause)
  (car clause))

(define (cond-actions clause)
  (cdr clause))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false
      (let ((first (car clauses))
	    (rest (cdr clauses)))
	(if (cond-else-clause? first)
	    (if (null? rest)
		(sequence->exp (cond-actions first))
		(error "ELSE clause isn't last: COND->IF" clauses))
	    (make-if (cond-predicate first)
		     (sequence->exp (cond-actions first))
		     (expand-clauses rest))))))

;; Exercise 4.2
(define (louis-application? exp)
  (tagged-list? exp 'call))

(define (louis-operator exp)
  (cadr exp))

(define (louis-operands exp)
  (cddr exp))

;; Exercise 4.3
(define (dd-eval exp env)
  (cond ((self-evaluating? exp) exp)
	((has-package? (car exp))
	 ((get 'eval (car exp)) exp env))
	((application? exp)
	 (sicp-apply (dd-eval (operator exp) env)
		     (list-of-values (operands exp) env)))
	(else
	 (error "Unknown expression type: DD-EVAL" exp))))

;; Exercise 4.4
(define (and? exp)
  (tagged-list? exp 'and))

(define (and-clauses exp)
  (cdr exp))

(define (eval-and exp env)
  (define (A clauses)
    (cond ((null? clauses) 'true)
	  ((false? (sicp-eval (car clauses) env)) 'false)
	  (else (A (cdr clauses)))))
  (A (and-clauses exp)))

(define (or? exp)
  (tagged-list? exp 'or))

(define (or-clauses exp)
  (cdr exp))

(define (eval-or exp env)
  (define (O clauses)
    (cond ((null? clauses) #f)
	  ((true? (sicp-eval (car clauses) env)) 'true)
	  (else (O (cdr clauses)))))
  (O (or-clauses exp)))

;; Exercise 4.5
(define (additional-syntax-clause? clause)
  (eq? (car (cond-actions? clause)) '=>))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false
      (let ((first (car clauses))
	    (rest (cdr clauses)))
	(cond ((cond-else-clause? first)
	       (if (null? rest)
		(sequence->exp (cond-actions first))
		(error "ELSE clause isn't last: COND->IF" clauses)))
	      ((additional-syntax-clause? first)
	       (make-if (cond-predicate first)
			(cons (cadr (cond-actions first))
			      (cond-predicate first))))
	      (else
	       (make-if (cond-predicate first)
			(sequence->exp (cond-actions first))
			(expand-clauses rest)))))))

;; Exercise 4.6
(define (let? exp)
  (tagged-list? exp 'let))

(define (let-assignments exp)
  (cadr exp))

(define (let-body exp)
  (caddr exp))

(define (let->combination exp)
  (cons
   (make-lambda (map car (let-assignments exp))
		(let-body exp))
   (map cdr (let-assignments exp))))

;; Exercise 4.7
(define (let*? exp)
  (tagged-list? exp 'let*))

(define (make-let assignments body)
  (list 'let assignments body))

(define (let*->nested-lets exp)
  (let ((assignments (let-assignments exp))
	(body (let-body exp)))
    (define (tnl assignments)
      (if (null? assignments)
	  body
	  (make-let (list (car assignments))
		    (tnl (cdr assignments)))))
    (tnl assignments)))

;; Exercise 4.8
(define (named-let? exp)
  (and (let? exp)
       (symbol? (cadr exp))))

(define (let-assignments exp)
  (if (named-let? exp)
      (caddr exp)
      (cadr exp)))

(define (let-body exp)
  (if (named-let? exp)
      (cadddr exp)
      (caddr exp)))

(define (let-name exp)
  (cadr exp))

(define (let->combination exp)
  (if (named-let? exp)
      (let ((lambda-args (cons (let-name exp)
			       (map assignment-variable
				    (let-assignments exp)))))
	(cons (make-lambda lambda-args lambda-args)
	      (cons (make-lambda (cdr lambda-args)
				 (let-body exp))
		    (map assignment-value
			 (let-assignments exp)))))
      (cons
       (make-lambda (map assignment-variable
			 (let-assignments exp))
		    (let-body exp))
       (map assignment-value (let-assignments exp)))))

;; Exercise 4.9
(define (while? exp)
  (tagged-list? exp 'while))

(define (while-cond exp)
  (cadr exp))

(define (while-body exp)
  (caddr exp))

(define (eval-while exp env)
  (if (true? (sicp-eval (while-cond exp) env))
      (begin (sicp-eval (while-body exp) env)
	     (eval-while exp env))
      'done))


;; Evaluator Data Structures
(define (true? exp)
  (not (false? exp)))

(define (false? exp)
  (eq? exp 'false))

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))

(define (procedure-parameters p)
  (cadr p))

(define (procedure-body p)
  (caddr p))

(define (procedure-environment p)
  (cadddr p))

(define (enclosing-environment env)
  (cdr env))

(define (first-frame env)
  (car env))

(define the-empty-environment '())

(define (make-frame variables values)
  (cons variables values))

(define (frame-variables frame)
  (car frame))

(define (frame-values frame)
  (cdr frame))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
  (cond ((= (length vars) (length vals))
	 (cons (make-frame vars vals) base-env))
	((< (length vars) (length vals))
	 (error "Too many arguments supplied: EXTEND-ENVIRONMENT"
		vars vals))
	(else
	 (error "Too few arguments supplied: EXTEND-ENVIRONMENT"
		vars vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
	     (env-loop (enclosing-environment env)))
	    ((eq? var (car vars)) (car vals))
	    (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
	(error "Unbound variable: LOOKUP-VARIABLE-VALUE" var)
	(let ((frame (first-frame env)))
	  (scan (frame-variables frame)
		(frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
	     (env-loop (enclosing-environment env)))
	    ((eq? var (car vars)) (set-car! vals val))
	    (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
	(error "Unbound variable: SET-VARIABLE-VALUE!" var)
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
    (scan (frame-variables frame) (frame-values frame))))
