(define apply-in-underlying-scheme apply)

(define (tagged-list? exp tag)
  (and (pair? exp)
       (eq? (car exp) tag)))

(define (self-evaluating? exp)
  (or (number? exp)
      (string? exp)
      (char? exp)))

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp)
  (cadr exp))

(define (variable? exp)
  (symbol? exp))

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

(define (and? exp)
  (tagged-list? exp 'and))

(define (and-clauses exp)
  (cdr exp))

(define (or? exp)
  (tagged-list? exp 'or))

(define (or-clauses exp)
  (cdr exp))

(define (let? exp)
  (tagged-list? exp 'let))

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
  (map list variables values))

(define (frame-variables frame)
  (map car frame))

(define (frame-values frame)
  (map cdr frame))

(define (add-binding-to-frame! var val frame)
  (set-cdr! frame (cons (car frame) (cdr frame)))
  (set-car! frame (list var val)))

(define (extend-environment vars vals base-env)
  (cond ((= (length vars) (length vals))
	 (cons (make-frame vars vals) base-env))
	((< (length vars) (length vals))
	 (error "Too many arguments supplied: EXTEND-ENVIRONMENT"
		vars vals))
	(else
	 (error "Too few arguments supplied: EXTEND-ENVIRONMENT"
		vars vals))))

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
	(list 'not not)
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
	((amb? exp) (analyse-amb exp))
	((application? exp) (analyse-application exp))
	(else (error "Unknown expression type: ANALYSE" exp))))

(define (amb? exp)
  (tagged-list? exp 'amb))

(define (amb-choices exp)
  (cdr exp))

(define (amb-eval exp env succeed fail)
  ((analyse exp) env succeed fail))

(define (analyse-self-evaluating exp)
  (lambda (env succeed fail)
    (succeed exp fail)))

(define (analyse-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env succeed fail)
      (succeed qval fail))))

(define (analyse-variable exp)
  (lambda (env succeed fail)
    (succeed (lookup-variable-value exp env) fail)))

(define (analyse-lambda exp)
  (let ((vars (lambda-parameters exp))
	(bproc (analyse-sequence (lambda-body exp))))
    (lambda (env succeed fail)
      (succeed (make-procedure vars bproc env) fail))))

(define (analyse-if exp)
  (let ((pproc (analyse (if-predicate exp)))
	(cproc (analyse (if-consequent exp)))
	(aproc (analyse (if-alternative exp))))
    (lambda (env succeed fail)
      (pproc env
	     (lambda (pred-value fail2)
	       (if (true? pred-value)
		   (cproc env succeed fail2)
		   (aproc env succeed fail2)))
	     fail))))

(define (analyse-sequence exps)
  (define (sequentially a b)
    (lambda (env succeed fail)
      (a env
	 (lambda (a-value fail2)
	   (b env succeed fail2))
	 fail)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
	first-proc
	(loop (sequentially first-proc
			    (car rest-procs))
	      (cdr rest-procs))))
  (let ((procs (map analyse exps)))
    (if (null? procs)
	(error "Empty sequence: ANALYSE")
	(loop (car procs) (cdr procs)))))

(define (analyse-definition exp)
  (let ((var (definition-variable exp))
	(vproc (analyse (definition-value exp))))
    (lambda (env succeed fail)
      (vproc env
	     (lambda (val fail2)
	       (define-variable! var val env)
	       (succeed 'ok fail2))
	     fail))))

(define (analyse-assignment exp)
  (let ((var (assignment-variable exp))
	(vproc (analyse (assignment-value exp))))
    (lambda (env succeed fail)
      (vproc env
	     (lambda (val fail2)
	       (let ((old-value
		      (lookup-variable-value var env)))
		 (set-variable-value! var val env)
		 (succeed 'ok
			  (lambda ()
			    (set-variable-value!
			     var old-value env)
			    (fail2)))))
	     fail))))

(define (get-args aprocs env succeed fail)
  (if (null? aprocs)
      (succeed '() fail)
      ((car aprocs)
       env
       (lambda (arg fail2)
	 (get-args (cdr aprocs)
		   env
		   (lambda (args fail3)
		     (succeed (cons arg args) fail3))
		   fail2))
       fail)))

(define (execute-application proc args succeed fail)
  (cond ((primitive-procedure? proc)
	 (succeed (apply-primitive-procedure proc args)
		  fail))
	((compound-procedure? proc)
	 ((procedure-body proc)
	  (extend-environment
	   (procedure-parameters proc)
	   args
	   (procedure-environment proc))
	  succeed
	  fail))
	(else
	 (error "Unknown procedure type: EXECUTE-APPLICATION" proc))))

(define (analyse-application exp)
  (let ((fproc (analyse (operator exp)))
	(aprocs (map analyse (operands exp))))
    (lambda (env succeed fail)
      (fproc env
	     (lambda (proc fail2)
	       (get-args aprocs
			 env
			 (lambda (args fail3)
			   (execute-application
			    proc args succeed fail3))
			 fail2))
	     fail))))

(define (analyse-amb exp)
  (let ((cprocs (map analyse (amb-choices exp))))
    (lambda (env succeed fail)
      (define (try-next choices)
	(if (null? choices)
	    (fail)
	    ((car choices)
	     env
	     succeed
	     (lambda ()
	       (try-next (cdr choices))))))
      (try-next cprocs))))

(define the-global-environment (setup-environment))

(define input-prompt ";;; Amb-Eval input:")
(define output-prompt ";;; Amb-Eval value:")

(define (driver-loop)
  (define (internal-loop try-again)
    (prompt-for-input input-prompt)
    (let ((input (read)))
      (if (eq? input 'try-again)
	  (try-again)
	  (begin
	    (newline)
	    (display ";;; Starting a new problem ")
	    (amb-eval
	     input
	     the-global-environment
	     (lambda (val next-alternative)
	       (announce-output output-prompt)
	       (user-print val)
	       (internal-loop next-alternative))
	     (lambda ()
	       (announce-output
		";;; There are no more values of")
	       (user-print input)
	       (driver-loop)))))))
  (internal-loop
   (lambda ()
     (newline)
     (display ";;; There is no current problem")
     (driver-loop))))

(driver-loop)
