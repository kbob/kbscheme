(define (last list)
  (if (null? list)
      '()
      (if (pair? list)
	  (if (null? (cdr list))
	      (car list)
	      (last (cdr list)))
	  list)))

(define (trace . exps)
  (draft-print exps 76)
  ;(draft-print exps)
  (last exps))

(define (notrace . exps)
     (last exps))

; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ;
; ; ; Standard procedures

(define (list . args)
  args)

(define (list? obj)			; detects cycles
  (define (list-0? obj trail)
    (if (eq? obj trail)
	#f
	(if (null? obj)
	    #t
	    (if (pair? obj)
		(list-1? (cdr obj) trail)
		#f))))
  (define (list-1? obj trail)
    (if (eq? obj trail)
	#f
	(if (null? obj)
	    #t
	    (if (pair? obj)
		(list-0? (cdr obj) (cdr trail))
		#f))))
  (list-0? obj (cons list-0? obj)))

(define (length list)
  (define (_ list count)
    (if (null? list)
	count
	(_ (cdr list) (+ count 1))))
  (_ list 0))

(define (reverse list)
  (define (_ list tail)
    (if (null? list)
	tail
	(_ (cdr list) (cons (car list) tail))))
  (_ list '()))

(define (map proc . lists)
  (define (cars lists)
    (if (null? lists)
	'()
	(cons (car (car lists)) (cars (cdr lists)))))
  (define (cdrs lists)
    (if (null? lists)
	'()
	(cons (cdr (car lists)) (cdrs (cdr lists)))))
  (define (_ proc lists result)
    (if (null? (car lists))
	(reverse result)
	(_ proc (cdrs lists) (cons (apply proc (cars lists)) result))))
  (_ proc lists '())
)

(define (list->vector list)
  (define v (make-vector (length list)))
  (define (vset! i list)
    (if (null? list)
	(if #f #f)
	(begin
	  (vector-set! v i (car list))
	  (vset! (+ i 1) (cdr list)))))
  (vset! 0 list)
  v)

(define (vector . objs)
  (list->vector objs))

(define (vector-map proc . vectors)
  (define (vector-refs vectors i)
    (if (null? vectors)
	'()
	(cons (vector-ref (car vectors) i)
	      (vector-refs (cdr vectors) i))))
  (define (_ i)
    (if (= i (vector-length (car vectors)))
	'()
	(cons (apply proc (vector-refs vectors i))
	      (_ (+ i 1)))))
  (list->vector (_ 0)))

(define (_accum-cmp accum cmp obj list)
  (accum (lambda (x) (cmp x obj)) list))

(define (assp proc alist)
  (if (null? alist)
      #f
      (if (proc (car (car alist)))
	  (car alist)
	  (assp proc (cdr alist)))))

(define (assq obj alist)
  (_accum-cmp assp eq? obj alist))

(define (fold-left combine nil . lists)
  (define (empties? lists)
    (if (null? lists)
	#t
	(null? (car lists))))
  (define (cars lists)
    (if (null? lists)
	'()
	(cons (car (car lists)) (cars (cdr lists)))))
  (define (cdrs lists)
    (if (null? lists)
	'()
	(cons (cdr (car lists)) (cdrs (cdr lists)))))
  (if (empties? lists)
      nil
      (apply combine
	     (apply fold-left combine nil (cdrs lists))
	     (cars lists))))

; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ;
; ; ; Low-Level Data

(define (make-environment parent)
  (cons '() parent))

(define env-bindings car)
(define env-parent cdr)
(define env-set-bindings! set-car!)

(define (env-add-binding! env binding)
  (env-set-bindings! env (cons binding (env-bindings env))))

(define (env-lookup env name)
  (define (_ seg env)
    (if (null? seg)
	(if (null? env)
	    #f
	    (_ (car env) (cdr env)))
	(letrec* ([binding (car seg)])
		 (if (eq? (binding-name binding) name)
		     binding
		     (_ (cdr seg) env)))))
  (_ '() env))

; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ;
; ; ; Expander

(define (make-label)
  (list "label"))

(define (label? obj)
  (if (pair? obj)
      (if (string? (car obj))
	  (string=? "subst" (car obj))
	  #f)
      #f))

(define (make-subst sym marks label)
  (list "subst" sym marks label))

(define (subst? obj)
  (if (pair? obj)
      (if (string? (car obj))
	  (string=? "subst" (car obj))
	  #f)
      #f))

(define (subst-sym sub)
  (car (cdr sub)))

(define (subst-marks sub)
  (car (cdr (cdr sub))))

(define (subst-label sub)
  (car (cdr (cdr (cdr sub)))))

(define (make-mark)
  (list "mark"))

(define (mark? obj)
  (if (pair? obj)
      (if (string? (car obj))
	  (string=? (car obj) "mark")
	  #f)
      #f))

(define top-mark (make-mark))

(define (top-marked? wrap)
  (if (null? wrap)
      #f
      (if (eq? (car wrap) top-mark)
	  #t
	  (top-marked? (cdr wrap)))))

(define (syntax-error object message)
  (draft-print (list message object))
  (error #f "~a ~s" message (strip object)))

(define (identifier? obj)
  (if (syntax-object? obj)
      (symbol? (syntax-object-expr obj))
      #f))

(define (self-evaluating? obj)
  ; I want or.
  (if (boolean? obj)
      #t
      (if (number? obj)
	  #t
	  (if (char? obj)
	      #t
	      (if (string? obj)
		  #t
		  (if (vector? obj)
		      #t
		      (bytevector? obj)))))))

(define (add-mark mark x)
  (extend-wrap (list mark) x))

(define (add-subst id label x)
  (extend-wrap (list (make-subst
		      (syntax-object-expr id)
		      (wrap-marks (syntax-object-wrap id))
		      label))
	       x))

(define (extend-wrap wrap x)
  (if (syntax-object? x)
      (make-syntax-object
       (syntax-object-expr x)
       (join-wraps wrap (syntax-object-wrap x)))
      (make-syntax-object x wrap)))

(define (join-wraps wrap1 wrap2)
  (if (null? wrap1)
      wrap2
      (if (null? wrap2)
	  wrap1
	  (begin
	    (define (f w w*)
	      (if (null? w*)
		  (if (if (mark? w) (eq? (car wrap2 w)) #f)
		      (cdr wrap2)
		      (cons w wrap2))
		  (cons w (f (car w*) (cdr w*)))))
	    (f (car wrap1) (cdr wrap1))))))

(define (id-binding id r)
  (label-binding id (id-label id) r))

(define (id-label id)
  (letrec*
   ([sym (syntax-object-expr id)]
    [wrap (syntax-object-wrap id)]
    [marks (wrap-marks wrap)])
   (define (search seg wrap)
     (if (null? seg)
	 (if (null? wrap)
	     (syntax-error id
			   (string-append
			    "undefined identifier: "
			    (symbol->string (syntax-object-expr id))))
	     (search (car wrap) (cdr wrap)))
	 (letrec* ([w0 (car seg)])
		  (if (mark? w0)
		      (search (cdr seg) wrap)
		      (if (if (eq? (subst-sym w0) sym)
			      (same-marks? (subst-marks w0) marks)
			      #f)
			  (subst-label w0)
			  (search (cdr seg) wrap))))))
	 
   (search '() wrap)))

(define (wrap-marks wrap)
  (define (_ wrap seg marks)
    (if (null? seg)
	(if (null? wrap)
	    (reverse marks)
	    (_ (cdr wrap) (car wrap) marks))
	(_ wrap
	   (cdr seg)
	   (if (mark? (car seg))
	       (cons (car seg) marks)
	       marks))))
  (_ wrap '() '()))

(define (same-marks? m1* m2*)
  (if (null? m1*)
      (null? m2*)
      (if (not (null? m2*))
	  (if (eq? (car m1*) (car m2*))
	      (same-marks? (cdr m1*) (cdr m2*))
	      #f)
	  #f)))

(define (label-binding id label r)
  ; I want or.
  ; (or (env-lookup r label) (syntax-error ...))
  (letrec* ([binding (env-lookup r label)])
	   (if binding
	       binding
	       (syntax-error id "displaced lexical"))))

; exp cases
;   x lexical identifier
;   x application
;     core form
;       define
;       define-syntax
;       quote
;       lambda
;       if
;       set!
;       letrec*
;       begin
;     macro use
;     pair

; My special forms:
;       define		?
;       define-syntax	?
;       quote		strip
;       lambda		extend env
;       letrec*		extend env
;       if		expand
;       set!		expand
;       begin		expand

; Dybvig's core forms:
;     quote
;y       calls strip
;     if
;y       calls exp
;     lambda
;y       pushes onto env
;     let
;y       pushes onto env
;     letrec-syntax
;y       expands, pushes onto meta-env
;     syntax
;y	just returns its argument
;     list
;	calls exp

(define (exp x r mr)
  (define (identifier-form? x)
    (if (syntax-pair? x)
	(identifier? (syntax-car x))
	#f))
  (define (exp-identifier id)
    (letrec* ([b (id-binding id r)]
	      [bt (binding-type b)])
	     (if (eq? bt (binding-macro))
		 (exp (exp-macro (binding-value b) x) r mr)
		 (if (eqv? bt (binding-lexical))
		     id;(binding-value b)
		     (syntax-error x "invalid syntax A")))))
 (define (exp-identifier-form x)
    (letrec* ([b (id-binding (syntax-car x) r)]
	      [bt (binding-type b)])
	     (if (eqv? bt (binding-macro))
		 (exp (exp-macro (binding-value b) x) r mr)
		 (if (eqv? bt (binding-lexical))
		     (cons (binding-value b) (exp-exprs (syntax-cdr x) r mr))
		     (if (eqv? bt (binding-core))
			 (exp-core (binding-value b) x r mr)
			 (syntax-error x "invalid syntax B"))))))
  (define (exp-pair x)
    (cons (exp (car x) r mr) (exp (cdr x) r mr)))
  (define (exp-other x)
    (letrec* ([d (strip x)])
	     (if (self-evaluating? d)
		 x;d
		 (syntax-error x "invalid syntax C"))))
  ; I want cond.
  (if (identifier? x)
      (exp-identifier x)
      (if (identifier-form? x)
	  (exp-identifier-form x)
	  (if (syntax-pair? x)
	      (exp-pair x)
	      (exp-other x)))))

(define (exp-macro p x)
  (letrec* ([m (make-mark)])
	   (add-mark m (p (add-mark m x)))))

(define (exp-core p x r mr)
  (p x r mr))

(define (exp-exprs x* r mr)
  (if (null? (syntax-object-expr x*))
      '()
      (cons (exp (syntax-car x*) r mr)
	    (exp-exprs (syntax-cdr x*) r mr))))

(define (exp-quote x r mr)
  (list 'quote (strip (syntax-car (syntax-cdr x)))))

(define (exp-lambda x r mr)
  ...)

(define (exp-define x r mr)
  ...)

(define (exp-define-syntax x r mr)
  ...)

(define (exp-letrec* x r mr)
  ...)

(define (syntax-pair? x)
  (pair? (syntax-object-expr x)))

(define (syntax-car x)
  (extend-wrap
   (syntax-object-wrap x)
   (car (syntax-object-expr x))))

(define (syntax-cdr x)
  (extend-wrap
   (syntax-object-wrap x)
   (cdr (syntax-object-expr x))))

(define (syntax x) '())

(define (exp-syntax x r mr)
  (syntax-car (syntax-cdr x)))

(define (strip stx)
  (if (syntax-object? stx)
      (if (top-marked? (syntax-object-wrap stx))
	  (syntax-object-expr stx)
	  (strip (syntax-object-expr stx)))
      (if (pair? stx)
	  (letrec* ([a (strip (car stx))]
		    [d (strip (cdr stx))])
		   (if (if (eq? a (car stx)) (eq? d (cdr stx)) #f)
		       stx
		       (cons a d)))
	  ; XXX handle vector too
	  stx)))

(define syntax->datum strip)

#;(define *wne-cache* '(() ()))

#;(define (initial-wrap-and-env env)
  (define specials
    '((quote . exp-quote)
      (syntax . exp-syntax)
      (lambda . exp-lambda)
      (define . exp-define)
      (define-syntax . exp-define-syntax)
      (letrec* . exp-letrec*)))
  (define (choose-binding-type binding)
    (if (special-form? (binding-value binding))
	(binding-core)
	(binding-lexical)))
  (define (bind-one sym)
    (letrec* ([sb (assq sym specials)])
	     (if sb
		 (make-binding
		  sym
		  (binding-core)
		  (binding-immutable)
		  (eval (cdr sb) (the-environment)))
		 (make-binding sym (binding-lexical) (binding-immutable) sym))))
  (define (wrap-one sym label parent)
    (cons (make-subst sym (list top-mark) label)
	  parent))
  (define (_ frame env)
    (if (null? frame)
	(if (null? env)
	    (list (list top-mark) '())
	    (_ (car env) (cdr env)))
	(if (eq? frame (car *wne-cache*))
	    (cdr *wne-cache*)
	    (letrec* ([pwne (_ (cdr frame) env)]
; bind name to itself
		      [orig-binding (car frame)]
		      [sym (binding-name orig-binding)]
		      #;[binding (make-binding sym
					     ;(choose-binding-type orig-binding)
					     (binding-lexical)
					     (binding-immutable)
					     sym)]

		      [binding (bind-one sym)]
; use original binding
#|
 |		      [binding (car frame)]
 |		      [sym (binding-name binding)]
 |#
; endif
		      [label (make-label)]
		      [wrap (wrap-one sym label (car pwne))]
		      [meta-env (cons (cons label binding) (cdr pwne))]
		      [wne (cons wrap meta-env)])
		     (set! *wne-cache* (cons frame wne))
		     wne))))
  (_ '() env))

; list of ((env1 . bindings) . (wrap . env2))
; where env1 is the eval-time env and env2 is the expand-time env.

(define *wne-cache* '())

(define (wnec-lookup env)
  (assp (lambda (a) (eq? (car a) env))
	*wne-cache*))

(define (wnec-update! env bindings wne)
  (letrec* ([a (wnec-lookup env)])
	   (if a
	       (begin
		 (set-cdr! (car a) bindings)
		 (set-cdr! a wne))
	       (set! *wne-cache*
		     (list (cons (cons env (env-bindings env)) wne))))))
	       

(define (initial-wrap-and-env env)
  (define specials
    (map
     (lambda (a)
       (cons (eval (car a) (the-environment))
	     (eval (cdr a) (the-environment))))
     '((quote . exp-quote)
       (syntax . exp-syntax)
       (lambda . exp-lambda)
       (define . exp-define)
       (define-syntax . exp-define-syntax)
       (letrec* . exp-letrec*))))
  (define (wrap-one sym label)
	   (make-subst sym (list top-mark) label))
  (define (bind-one binding label)
    (letrec*
     ([sym (binding-name binding)]
      [value (binding-value binding)]
      [sb (assq value specials)])
     (if sb
	 (make-binding label (binding-core) (binding-immutable) (cdr sb))
	 (make-binding label (binding-lexical) (binding-immutable) sym))))
  (define (make-one-wne binding)
    (define label (make-label))
    (cons (wrap-one (binding-name binding) label)
	  (bind-one binding label)))
  (define (make-frame-wne bindings cached)
    (if (null? bindings)
	'(() ())
	;(list (list top-mark) '())
	(if (if cached (eq? bindings (cdr (car cached))) #f)
	    (cdr cached)
	    (letrec* ([first-wne (make-one-wne (car bindings))]
		      [rest-wnes (make-frame-wne (cdr bindings) cached)])
		     (cons
		      (cons (car first-wne) (car rest-wnes))
		      (cons (cdr first-wne) (cdr rest-wnes)))))))
  (define (make-env-wne env)
    (letrec* ([cached (wnec-lookup env)]
	      [bindings (env-bindings env)])
	     (if (if cached (eq? (cdr (car cached)) bindings) #f)
		 (cdr cached)
		 (letrec* ([new-wne (make-frame-wne bindings cached)])
			  (wnec-update! env (env-bindings env) new-wne)
			  new-wne))))

  (if (null? env)
      (list (list (list top-mark)) '())
      (letrec* ([first-ewne (make-env-wne env)]
		[rest-ewne (initial-wrap-and-env (env-parent env))])
	       (cons (cons (car first-ewne) (car rest-ewne))
		     (cons (cdr first-ewne) (cdr rest-ewne))))))

(define (expand form env)
  (letrec* ([wne (initial-wrap-and-env env)]
	    [wrap (car wne)]
	    [meta-env (cdr wne)])
	   (syntax->datum
	    (exp (make-syntax-object form wrap) meta-env meta-env))))

123
list
(list 1 2 3)
(draft-print 123)
(if 1 2 3)
(draft-print (if 1 2 3))
(quote abc)
(draft-print (quote abc))
(draft-print (quote (a b c)))
(syntax 123)

#|
; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ;
; ; ; Standard syntax

(define-syntax syntax-case
  (lambda (x)
    ???))

(define-syntax with-syntax
  (lambda (x)
    (syntax-case x ()
      [(_ ((p e0) ...) e1 e2 ...)
       (syntax (syntax-case (list e0 ...) ()
		 [(p ...) (begin e1 e2 ...)]))])))

(define-syntax syntax-rules
  (lambda (x)
    (syntax-case x ()
      [(_ (i ...) ((keyword . pattern) template) ...)
       (syntax (lambda (x)
		 (syntax-case x (i ...)
		   [(dummy . pattern) (syntax template)]
		   ...)))])))

#;(define-syntax let
    (syntax-rules ()
      [(_ ((var init) ...)		; unnamed let
	  body1 body2 ...)
       ((lambda (var ...)
	  body1 body2 ...)
	init ...)]
      [(_ label				; named let
	  ((var init) ...)
	  body1 body2 ...)
       ((lambda (label)
	  (set! label (lambda (var ...)
			body1 body2 ...))
	  (label init ...))
	'())]))

#;(define-syntax cond
  (syntax-rules (else =>)
    [(_ (else result1 result2 ...))
     (begin result1 result2 ...)]
    [(_ (test => result))
     (let ((temp test))
       (if temp (result temp)))]
    [(_ (test => result) clause1 claues2 ...)
     (let ((temp test))
       (if temp
	   (result temp)
	   (cond clause1 clause2 ...)))]
    [(_ (test))
     test]
    [(_ (test) clause1 clause2 ...)
	   (let ((temp test))
	     (if temp)
	     temp
	     (_ (clause1 clause2 ...)))]
    [(_ (test result1 result2 ...))
     (if test (begin result1 result2 ...))]
    [(_ (test result1 result2 ...) clause1 clause2 ...)
     (if test
	 (begin result1 result2 ...)
	 (cond clause1 clause2 ...))]))

#; (define-syntax case ...)

#; (define-syntax let* ...)

(define-syntax letrec			; from r6rs-lib 12.7
  (lambda (x)
    (syntax-case x ()
      [(_ ((i e) ...) b1 b2 ...)
       (with-syntax
	[((t ...) (generate-temporaries #'(i ...)))]
	#'(let ((i '()) ...)
	    (let ((t e) ...)
	      (set! i t) ...
	      (let () b1 b2 ...))))])))

#; (define-syntax letrec* ...)

#; (define-syntax let-values ...)

#; (define-syntax let*-values ...)

#;(define-syntax and
  (syntax-rules ()
    [(_) #f]
    [(_ e1) e1]
    [(_ e1 e2 ...) (let ((t e1))
		     (if t (and e2 ...) #f))]))

#;(define-syntax or
  (syntax-rules ()
    [(_) #t]
    [(_ e1) e1]
    [(_ e1 e2 ...) (let ((t e1))
		     (if t t (or e2 ...)))]))

; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ;
; ; ; Libraries

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
      (env-add-binding! env (make-binding name type mutability value))))

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
    ; I want unless.
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

; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ;
; ; ; The REPL (someday)

(define (repl)
  ((lambda (x)
     (if (not (eof-object? x))
	 ((lambda ()
	    (draft-print (eval x (environment '(trivial))))
	    (repl)))))
   (draft-read)))

#;(repl)

#;(draft-print "hello again")
|#
