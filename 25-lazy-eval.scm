;; Evaluation and Analysis
(define (s-eval exp env)
  (cond ((self-evaluating? exp) exp)
	((variable? exp) (lookup-variable-value exp env))
	((quoted? exp) (text-of-quotation exp))
	((assignment? exp) (eval-assignment exp env))
	((definition? exp) (eval-definition exp env))
	((if? exp) (eval-if exp env))
	((lambda? exp) (make-procedure (lambda-parameters exp)
				       (lambda-body exp)
				       env))
	((begin? exp)
	 (eval-sequence (begin-actions exp) env))
	((cond? exp) (s-eval (cond->if exp) env))
	((let? exp) (s-eval (let->combination exp) env))
	((let*? exp) (s-eval (let*->nested-lets exp) env))
	((and? exp) (eval-and exp env))
	((or? exp) (eval-or exp env))
	((application? exp)
	 (s-apply (actual-value (operator exp) env)
		  (operands exp)
		  env))
	(else
	 (error "Unknown expression type: S-EVAL" exp))))

(define (eval-if exp env)
  (if (true? (actual-value (if-predicate exp) env))
      (s-eval (if-consequent exp) env)
      (s-eval (if-alternative exp) env)))

(define (eval-sequence exps env)
  (cond ((last-exp? exps)
	 (s-eval (first-exp exps) env))
	(else
	 (s-eval (first-exp exps) env)
	 (eval-sequence (rest-exps exps) env))))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
		       (s-eval (assignment-value exp) env)
		       env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
    (s-eval (definition-value exp) env)
    env)
  'ok)

;; Application
(define apply-in-underlying-scheme apply)

(define (s-apply procedure arguments env)
  (cond ((primitive-procedure? procedure)
	 (apply-primitive-procedure
	  procedure
	  (list-of-arg-values arguments env)))
	((compound-procedure? procedure)
	 (eval-sequence
	  (procedure-body procedure)
	  (extend-environment
	   (procedure-parameters procedure)
	   (list-of-delayed-args arguments env)
	   (procedure-environment procedure))))
	(else (error "Unknown procedure type: S-APPLY" procedure))))

;; Laziness
(define (force-it obj)
  (cond ((thunk? obj)
	 (let ((result (actual-value (thunk-exp obj)
				     (thunk-env obj))))
	   (set-car! obj 'evaluated-thunk)
	   (set-car! (cdr obj) result)
	   (set-cdr! (cdr obj) '())
	   result))
	((evaluated-thunk? obj) (thunk-value obj))
	(else obj)))

(define (delay-it exp env)
  (list 'thunk exp env))

(define (thunk? obj)
  (tagged-list? obj 'thunk))

(define (thunk-exp thunk)
  (cadr thunk))

(define (thunk-env thunk)
  (caddr thunk))

(define (evaluated-thunk? obj)
  (tagged-list? obj 'evaluated-thunk))

(define (thunk-value evaluated-thunk)
  (cadr evaluated-thunk))

(define (actual-value exp env)
  (force-it (s-eval exp env)))

(define (list-of-arg-values exps env)
  (if (no-operands? exps)
      '()
      (cons (actual-value (first-operand exps)
			  env)
	    (list-of-arg-values (rest-operands exps)
				env))))

(define (list-of-delayed-args exps env)
  (if (no-operands? exps)
      '()
      (cons (delay-it (first-operand exps)
		      env)
	    (list-of-delayed-args (rest-operands exps)
				  env))))

;; Typing
(define (tagged-list? exp tag)
  (and (pair? exp)
       (eq? (car exp) tag)))

;; Basic Expressions
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

;; Procedures
(define (lambda? exp)
  (tagged-list? exp 'lambda))

(define (lambda-parameters exp)
  (cadr exp))

(define (lambda-body exp)
  (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define primitive-procedures
  (list (list 'car car)
	(list 'cdr cdr)
	(list 'cons cons)
	(list 'null? null?)
	(list 'list list)
	(list '+ +)
	(list '- -)
	(list '* *)
	(list '/ /)
        (list '1+ 1+)
	(list '1- 1-)
	(list '= =)
	(list '< <)
	(list '> >)
	(list 'eq? eq?)
	(list 'equal? equal?)
	(list 'number? number?)
	(list 'string? string?)
	(list 'symbol? symbol?)
	(list 'cadr cadr)
	(list 'cddr cddr)
	(list 'caddr caddr)
	(list 'cdddr cdddr)
	(list 'cadddr cadddr)
	(list 'cddddr cddddr)
	(list 'set-car! set-car!)
	(list 'set-cdr! set-cdr!)
	(list 'exit exit)))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc)
  (cadr proc))

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc)
   args))

(define (primitive-procedure-names)
  (map car primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

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

;; Application
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

;; Environment and Assignment
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

(define (setup-environment)
  (let ((initial-env
	 (extend-environment (primitive-procedure-names)
			     (primitive-procedure-objects)
			     the-empty-environment)))
    (define-variable! 'true #t initial-env)
    (define-variable! 'false #f initial-env)
    initial-env))

(define (extend-environment vars vals base-env)
  (cond ((= (length vars) (length vals))
	 (cons (make-frame vars vals) base-env))
	((< (length vars) (length vals))
	 (error "Too many arguments supplied: EXTEND-ENVIRONMENT"
		vars vals))
	(else
	 (error "Too few arguments supplied: EXTEND-ENVIRONMENT"
		vars vals))))

(define (enclosing-environment env)
  (cdr env))

(define (first-frame env)
  (car env))

(define the-empty-environment '())

(define (make-frame variables values)
  (map list variables values))

(define (frame-variables frame)
  (map car frame))

(define (frame-values frame)
  (map cdr frame))

(define (add-binding-to-frame! var val frame)
  (set-cdr! frame (cons (car frame) (cdr frame)))
  (set-car! frame (list var val)))

(define (scan-frame frame var val update)
  (define (scan frame)
    (cond ((null? frame) #f)
	  ((eq? var (caar frame))
	   (if update
	       (begin
		 (set-cdr! (car frame) (list val))
		 (cons #t val))
	       (cons #t (cadar frame))))
	  (else (scan (cdr frame)))))
  (scan frame))

(define (search-frame frame var)
  (scan-frame frame var #f #f))

(define (update-frame frame var val)
  (scan-frame frame var val #t))

(define (for-each-env f env)
  (define (FEE env)
    (if (eq? env the-empty-environment)
	#f
	(let ((val (f (first-frame env))))
	  (if val
	      val
	      (FEE (enclosing-environment env))))))
  (FEE env))

(define (lookup-variable-value var env)
  (let ((val (for-each-env
	      (lambda (frame) (search-frame frame var))
	      env)))
    (if val
	(cdr val)
	(error "Unbound variable: LOOKUP-VARIABLE-VALUE" var))))

(define (set-variable-value! var val env)
  (let ((updated-val (for-each-env
	      (lambda (frame) (update-frame frame var val))
	      env)))
    (if (not updated-val)
        (error "Unbound variable: SET-VARIABLE-VALUE!" var))))

(define (define-variable! var val env)
  (let ((defined-val (update-frame (first-frame env) var val)))
    (if (not defined-val)
	(add-binding-to-frame! var val (first-frame env)))))

;; Lexical Bindings
(define (let? exp)
  (tagged-list? exp 'let))

(define (let*? exp)
  (tagged-list? exp 'let*))

(define (make-let assignments body)
  (append (list 'let assignments) body))

(define (let*->nested-lets exp)
  (let ((assignments (let-assignments exp))
	(body (let-body exp)))
    (define (tnl assignments)
      (if (null? assignments)
	  body
	  (make-let (list (car assignments))
		    (tnl (cdr assignments)))))
    (tnl assignments)))

(define (named-let? exp)
  (and (let? exp)
       (symbol? (cadr exp))))

(define (let-assignments exp)
  (if (named-let? exp)
      (caddr exp)
      (cadr exp)))

(define (let-body exp)
  (if (named-let? exp)
      (cdddr exp)
      (cddr exp)))

(define (let-assignment-variable exp)
  (car exp))

(define (let-assignment-value exp)
  (cadr exp))

(define (let-name exp)
  (cadr exp))

(define (let->combination exp)
  (if (named-let? exp)
      (let ((lambda-args (cons (let-name exp)
			       (map let-assignment-variable
				    (let-assignments exp)))))
	(cons (make-lambda lambda-args lambda-args)
	      (cons (make-lambda (cdr lambda-args)
				 (let-body exp))
		    (map let-assignment-value
			 (let-assignments exp)))))
      (cons
       (make-lambda (map let-assignment-variable
			 (let-assignments exp))
		    (let-body exp))
       (map let-assignment-value (let-assignments exp)))))

;; Booleans
(define (true? exp)
  (eq? exp #t))

(define (false? exp)
  (eq? exp #f))

(define (and? exp)
  (tagged-list? exp 'and))

(define (and-clauses exp)
  (cdr exp))

(define (eval-and exp env)
  (define (A clauses)
    (cond ((null? clauses) 'true)
	  ((false? (s-eval (car clauses) env)) 'false)
	  (else (A (cdr clauses)))))
  (A (and-clauses exp)))

(define (or? exp)
  (tagged-list? exp 'or))

(define (or-clauses exp)
  (cdr exp))

(define (eval-or exp env)
  (define (O clauses)
    (cond ((null? clauses) #f)
	  ((true? (s-eval (car clauses) env)) 'true)
	  (else (O (cdr clauses)))))
  (O (or-clauses exp)))

;; Sequences
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

;; Conditionals
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

;; REPL
(define input-prompt ";;; L-Eval input:")
(define output-prompt ";;; L-Eval value:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output
	   (actual-value
	    input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

(define (prompt-for-input str)
  (newline) (newline) (display str) (newline))

(define (announce-output str)
  (newline) (display str) (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list
		'compound-procedure
		(procedure-parameters object)
		(procedure-body object)
		'<procedure-env>))
      (display object)))

(define the-global-environment (setup-environment))

(driver-loop)
