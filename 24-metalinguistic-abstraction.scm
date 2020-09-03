(define apply-in-underlying-scheme apply)

;; You could call this simply "eval", but depending on the Scheme
;; implementation you use (I like Guile), hilarity may ensue.
(define (sicp-eval exp env)
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
  (eq? (car (cond-actions clause)) '=>))

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
			      (cond-predicate first))
			(expand-clauses rest)))
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
  (eq? exp #t))

(define (false? exp)
  (eq? exp #f))

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

;; Exercise 4.11
(define (make-frame variables values)
  (map list variables values))

(define (frame-variables frame)
  (map car frame))

(define (frame-values frame)
  (map cdr frame))

(define (add-binding-to-frame! var val frame)
  (set-cdr! frame (cons (car frame) (cdr frame)))
  (set-car! frame (list var val)))

;; Exercise 4.12
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

;; Exercise 4.13
(define (remove-binding-from-frame! var frame)
  (define (R frame)
    (cond ((null? frame)
	   'done)
	  ((eq? var (caar frame))
	   (set! frame (cdr frame))
	   (R frame))
	  (else (R (cdr frame)))))
  (R frame))

(define (make-unbound! var env)
  (remove-binding-from-frame! var (first-frame env)))


;; Running the Evaluator as a Program
(define (setup-environment)
  (let ((initial-env
	 (extend-environment (primitive-procedure-names)
			     (primitive-procedure-objects)
			     the-empty-environment)))
    (define-variable! 'true #t initial-env)
    (define-variable! 'false #f initial-env)
    initial-env))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc)
  (cadr proc))

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

(define (primitive-procedure-names)
  (map car primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))

(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (sicp-eval input the-global-environment)))
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

;; Exercise 4.16
(define (lookup-variable-value var env)
  (let ((val (for-each-env
	      (lambda (frame) (search-frame frame var))
	      env)))
    (if (and val
	     (not (eq? (cdr val) '*unassigned*)))
	(cdr val)
	(error "Unbound variable: LOOKUP-VARIABLE-VALUE" var))))

(define (scan-out-defines proc-body)
  (display proc-body)
  (newline)
  (define (SOD body definitions)
    (if (and (not (null? body))
	     (definition? (car body)))
	(SOD (cdr body) (append definitions (list (car body))))
	(make-let (map (lambda (d)
			 (list (definition-variable d)
			       ''*unassigned*))
		       definitions)
		  (append
		   (map (lambda (d)
			  (list 'set!
				(definition-variable d)
				(definition-value d)))
			definitions)
		   body))))
  (if (definition? (car proc-body))
      (list (SOD (cdr proc-body) (list (car proc-body))))
      proc-body))

(define (make-procedure parameters body env)
  (list 'procedure parameters (scan-out-defines body) env))


;; Separating Syntactic Analysis from Execution
(define (sicp-eval exp env)
  ((analyse exp) env))

(define (analyse exp)
  (cond ((self-evaluating? exp) (analyse-self-evaluating exp))
	((quoted? exp) (analyse-quoted exp))
	((variable? exp) (analyse-variable exp))
	((assignment? exp) (analyse-assignment exp))
	((definition? exp) (analyse-definition exp))
	((if? exp) (analyse-if exp))
	((lambda? exp) (analyse-lambda exp))
	((begin? exp) (analyse-sequence (begin-actions exp)))
	((cond? exp) (analyse (cond->if exp)))
	((application? exp) (analyse-application exp))
	(else (error "Unknown expression type: ANALYSE" exp))))

(define (analyse-self-evaluating exp)
  (lambda (env) exp))

(define (analyse-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env) qval)))

(define (analyse-variable exp)
  (lambda (env) (lookup-variable-value exp env)))

(define (analyse-assignment exp)
  (let ((var (assignment-variable exp))
	(vproc (analyse (assignment-value exp))))
    (lambda (env)
      (set-variable-value! var (vproc env) env)
      'ok)))

(define (analyse-definition exp)
  (let ((var (definition-variable exp))
	(vproc (analyse (definition-value exp))))
    (lambda (env)
      (define-variable! var (vproc env) env)
      'ok)))

(define (analyse-if exp)
  (let ((pproc (analyse (if-predicate exp)))
	(cproc (analyse (if-consequent exp)))
	(aproc (analyse (if-alternative exp))))
    (lambda (env) (if (true? (pproc env))
		      (cproc env)
		      (aproc env)))))

(define (analyse-lambda exp)
  (let ((vars (lambda-parameters exp))
	(bproc (analyse-sequence (lambda-body exp))))
    (lambda (env) (make-procedure vars bproc env))))

(define (analyse-sequence exps)
  (define (sequentially proc1 proc2)
    (lambda (env) (proc1 env) (proc2 env)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
	first-proc
	(loop (sequentially first-proc (car rest-procs))
	      (cdr rest-procs))))
  (let ((procs (map analyse exps)))
    (if (null? procs) (error "Empty sequence: ANALYSE"))
    (loop (car procs) (cdr procs))))

(define (execute-application proc args)
  (cond ((primitive-procedure? proc)
	 (apply proc args))
	((compound-procedure? proc)
	 ((procedure-body proc)
	  (extend-environment
	   (procedure-parameters proc)
	   args
	   (procedure-environment proc))))
	(else
	 (error "Unknown procedure type: EXECUTE-APPLICATION" proc))))

(define (analyse-application exp)
  (let ((fproc (analyse (operator exp)))
	(aprocs (map analyse (operands exp))))
    (lambda (env)
      (execute-application
       (fproc env)
       (map (lambda (aproc) (aproc env))
	    aprocs)))))

(define the-global-environment (setup-environment))

(driver-loop)
