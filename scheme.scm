(define (list . args)
  args)

(define (expand form env)
  #;(draft-print (list 'expanding form))
  form)

; An environment is implemented as a pair.  The car is a list of
; bindings, and the cdr is the parent environment.

(define (generate-temporaries l)
  (define (_ l temps)
    (if (null? l)
	temps
	(_ (cdr l) (cons (make-anonymous-symbol) temps))))
  (_ l '()))

(define (make-environment parent)
  (cons '() parent))

(define env-bindings car)
(define env-parent cdr)
(define env-set-bindings! set-car!)

(define (bound? env name)
  (define (_ bindings)
    (if (null? bindings)
	#f
	(if (eq? name (binding-name (car bindings)))
	    (car bindings)
	    (_ (cdr bindings)))))
  (_ (env-bindings env)))
	    
(define (env-bind env name type mutability value)
  (define binding (bound? env name))
  (if binding
      (binding-set! binding value)
      (env-set-bindings! env (cons (make-binding name type mutability value)
				   (env-bindings env)))))

; A library is implemented as a pair.  The car is the environment, and
; the cdr is the name spec, e.g., (rnrs (6)).

(define (make-library env namespec)
  (cons env namespec))

(define library-environment car)
(define library-namespec cdr)

(define *libraries* '())

(define (lists-equal? a b)
  (if (pair? a)
      (if (pair? b)
	  (if (lists-equal? (car a) (car b))
	      (lists-equal? (cdr a) (cdr b))
	      #f)
	  #f)
      (eqv? a b)))

(define (find-library namespec)
  (define (_ libs)
    (if (null? libs)
	(raise (make-undefined-violation))
	(if (lists-equal? namespec (library-namespec (car libs)))
	    (car libs)
	    (_ (cdr libs)))))
  (_ *libraries*))

(define (environment namespec)
  (library-environment (find-library namespec)))

(define root-environment (the-environment))

(define (define-library namespec libraries symbols)
  (define env
    (make-environment '()))
  (define (import-binding binding)
    (env-bind env
	      (binding-name binding)
	      (binding-type binding)
	      (binding-mutable)
	      (binding-value binding)))
  (define (import-bindings bindings)
    #;(unless (null? bindings)
	      (import-binding (car bindings))
	      (import-bindings (cdr bindings)))
    (if (null? bindings)
	(if #f #f)
	(begin (import-binding (car bindings))
	       (import-bindings (cdr bindings)))))
  (define (import-env imp-env)
    (if (null? imp-env)
	(if #f #f)
	(begin (import-bindings (env-bindings imp-env))
	       (import-env (env-parent imp-env)))))
  (define (import-lib lib)
    (import-env (environment lib)))
  (define (import-libs libs)
    (if (null? libs)
	(if #f #f)
	(begin (import-lib (car libs))
	       (import-libs (cdr libs)))))
  (define (import-symbols syms)
    (if (null? syms)
	(if #f #f)
	(begin (define sym (car syms))
	       (define binding (get-binding sym root-environment))
	       (env-bind env
			 sym
			 (binding-type binding)
			 (binding-mutable)
			 (binding-value binding))
	       (import-symbols (cdr syms)))))
  (define (freeze-bindings bindings)
    (if (null? bindings)
	(if #f #f)
	(begin (binding-set-mutability! (car bindings) (binding-immutable))
	       (freeze-bindings (cdr bindings)))))
  (import-libs libraries)
  (import-symbols symbols)
  (freeze-bindings (env-bindings env))
  (define lib (cons env namespec))
  (set! *libraries* (cons lib *libraries*))
  lib)

(define-library '(rnrs eval (6))
  '()
  '(eval
    environment))

(define-library
  '(trivial)
  '((rnrs eval (6)))
  '(cons
    car
    cdr
    generate-temporaries
    make-anonymous-symbol
    quote))

(define (repl)
  ((lambda (x)
     (if (not (eof-object? x))
	 ((lambda ()
	    (draft-print (eval x (environment '(trivial))))
	    (repl)))))
   (draft-read)))

#;(repl)

#;(draft-print "hello again")
