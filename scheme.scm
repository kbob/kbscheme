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

(define (append . lists)
  (define (_  list lists)
    (if (null? lists)
	list
	(if (null? list)
	    (_ (car lists) (cdr lists))
	    (cons (car list) (_ (cdr list) lists)))))
  (_ '() lists))

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

(define (remp proc list)
  (define (_ list result)
    (if (null? list)
	(reverse result)
	(_ (cdr list)
	   (if (proc (car list))
	       result
	       (cons (car list) result)))))
  (_ list '()))

(define (remove obj list)
  (_accum-cmp remp equal? obj list))

(define (remv obj list)
  (_accum-cmp remp eqv? obj list))

(define (remq obj list)
  (_accum-cmp remp eq? obj list))

(define (memp proc list)
  (if (null? list)
      #f
      (if (proc (car list))
	  list
	  (memp proc (cdr list)))))

(define (member obj list)
  (_accum-cmp memp equal? obj list))

(define (memv obj list)
  (_accum-cmp memp eqv? obj list))

(define (memq obj list)
  (_accum-cmp memp eq? obj list))

(define (assp proc alist)
  (if (null? alist)
      #f
      (if (proc (car (car alist)))
	  (car alist)
	  (assp proc (cdr alist)))))

(define (assoc obj alist)
  (_accum-cmp assp equal? obj alist))

(define (assv obj alist)
  (_accum-cmp assp eqv? obj alist))

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
; ; ; Environments

(define *root-environment* (the-environment))

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

(define (syntax-error object message)
  (draft-print (list message object))
  (error #f "~a ~s" message (strip object)))

; label		----------------------------------

(define *lc* 0)

(define (make-label)
  (set! *lc* (+ *lc* 1))
  (cons "label" *lc*))

(define (label? obj)
  (if (pair? obj)
      (if (string? (car obj))
	  (string=? "label" (car obj))
	  #f)
      #f))

; mark		----------------------------------

(define *mc* 0)

(define (make-mark)
  (set! *mc* (+ *mc* 1))
  (cons "mark" *mc*))

(define (mark? obj)
  (if (pair? obj)
      (if (string? (car obj))
	  (string=? (car obj) "mark")
	  #f)
      #f))

; markset	----------------------------------

#;(define (make-markset . marks)
  (apply list marks))
  (define make-markset list)

(define markset-has-mark? memq)

(define (marksets-equal? m1 m2)
  (if (eqv? (length m1) (length m2))
      (null? (join-marksets m1 m2))
      #f))

(define (join-marksets m1 m2)
  (define (in marks)
    (lambda (m) (markset-has-mark? m marks)))
  (append (remp (in m1) m2) (remp (in m2) m1)))

; subst		----------------------------------

(define (make-subst sym markset label)
  (vector "subst" sym markset label))

(define (subst-sym sub)
  (vector-ref sub 1))

(define (subst-markset sub)
  (vector-ref sub 2))

(define (subst-label sub)
  (vector-ref sub 3))

; substset	----------------------------------

(define make-substset list)

(define join-substsets append)

(define (substset-find substset sym markset)
  (if (null? substset)
      #f
      (letrec* ([sub (car substset)])
	       (if (if (eq? sym (subst-sym sub))
		       (marksets-equal? markset (subst-markset sub))
		       #f)
		   sub
		   (substset-find (cdr substset) sym markset)))))

; wrap		----------------------------------

(define (make-wrap markset substset)
  (vector "wrap" markset substset))

(define (wrap-markset wrap)
  (vector-ref wrap 1))

(define (wrap-substset wrap)
  (vector-ref wrap 2))

(define (join-wraps w1 w2)
  (make-wrap (join-marksets (wrap-markset w1) (wrap-markset w2))
             (join-substsets (wrap-substset w1) (wrap-substset w2))))

; syntax-object	-----------------------------

(define (extend-wrap wrap x)
  (if (syntax-object? x)
      (make-syntax-object
       (syntax-object-expr x)
       (join-wraps wrap (syntax-object-wrap x)))
      (make-syntax-object x wrap)))

(define (add-mark mark x)
  (extend-wrap (make-wrap (make-markset mark) (make-substset)) x))

(define (add-subst subst x)
  (extend-wrap (make-wrap (make-markset) (make-substset subst)) x))

(define (id-label id)
  (notrace 'id-label (syntax-object-expr id) '=>
  (letrec* ([sym (syntax-object-expr id)]
	    [XXX1 (notrace 'id-label 'sym sym)]
	    [wrap (syntax-object-wrap id)]
	    [XXX2-5 (notrace 'id-label 'wrap wrap)]
	    [XXX2-6 (notrace 'id-label '(length wrap) (length (wrap-substset  wrap)))]
	    [markset (wrap-markset wrap)]
	    [XXX3 (notrace 'id-label 'markset markset)]
	    [substset (wrap-substset wrap)]
	    [sub (substset-find substset sym markset)])
	   (if sub
	       (subst-label sub)
	       (syntax-error id
			     (string-append
			      "undefined identifier: "
			      (symbol->string (syntax-object-expr id)))))))
  )

(define top-mark (make-mark))

(define (top-marked? wrap)
  (markset-has-mark? top-mark (wrap-markset wrap)))

(define (identifier? obj)
  (notrace 'identifier? (syntax-object-expr obj) '=>
  (if (syntax-object? obj)
      (symbol? (syntax-object-expr obj))
      #f))
  )

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

(define (id-binding id r)
  (notrace 'id-binding (syntax-object-expr id) '=>
  (label-binding id (id-label id) r))
  )

(define (label-binding id label r)
  ; I want or.
  ; (or (env-lookup r label)
  ;     (syntax-error id "displaced lexical"))
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
	     (if (eqv? bt (binding-macro))
		 (exp (exp-macro (binding-value b) x) r mr)
		 (if (eqv? bt (binding-lexical))
		     (binding-value b)
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
    (cons (exp (syntax-car x) r mr) (exp (syntax-cdr x) r mr)))
  (define (exp-other x)
    (letrec* ([d (strip x)])
	     (notrace 'exp-other (syntax-object-expr x))
	     (if (self-evaluating? d)
		 x
		 (syntax-error x "invalid syntax C"))))
  (notrace 'exp (syntax-object-expr x))
  ; I want cond.
  (if (identifier? x)
      (exp-identifier x)
      (if (identifier-form? x)
	  (exp-identifier-form x)
	  (if (syntax-pair? x)
	      (exp-exprs x r mr)
	      (exp-other x)))))

(define (exp-macro p x)
  (letrec* ([m (make-mark)])
	   (add-mark m (p (add-mark m x)))))

(define (exp-core p x r mr)
  (p x r mr))

(define (exp-exprs x* r mr)
  (notrace 'exp-exprs (syntax-object-expr x*))
  (if (null? (syntax-object-expr x*))
      '()
      (cons (exp (syntax-car x*) r mr)
	    (exp-exprs (syntax-cdr x*) r mr))))

(define (exp-quote x r mr)
  (list 'quote (strip (syntax-car (syntax-cdr x)))))

; Get the arg list.
; Make a fresh anonymous symbol and a fresh label for each.
; Build a substition set mapping each one to a fresh label.
; Build list
;   (list 'lambda new-args splice-in (exp b new-env mr) for b in body)

(define (exp-lambda x r mr)
  (letrec* ([wrap (syntax-object-wrap x)]
	    [marks (wrap-markset wrap)]
	    [XXX (notrace 'marks marks)]
	    [new-ss '()]
	    [new-env (make-environment r)]
	    )
	    (define (make-arg id)
	      (letrec* ([sym (syntax-object-expr id)]
			[label (make-label)]
			[new-name (make-anonymous-symbol)])
		       (env-add-binding!
			new-env
			(make-binding label
				      (binding-lexical)
				      (binding-immutable)
				      new-name))
		       (set! new-ss
			     (cons
			      (make-subst sym marks label)
			      new-ss))
			new-name))
	    (define (traverse-args al)
	      (notrace 'traverse-args al '= (syntax-object-expr al))
	      (if (syntax-null? al)
		  '()
		  (if (syntax-pair? al)
		      (cons (make-arg (syntax-car al))
			    (traverse-args (syntax-cdr al)))
		      (make-arg al))))
	    (define lammie (syntax-car x))
	    (define args (syntax-car (syntax-cdr x)))
	    (define new-args (traverse-args args))
	    (define new-wrap (make-wrap (make-markset) new-ss))
	    ;(draft-print (list 'new-wrap new-wrap))
	    ;(draft-print (list 'new-env new-env) 300)
	    (define body (syntax-cdr (syntax-cdr x)))
	    (define new-body (exp-exprs (extend-wrap new-wrap body) new-env mr))
	    (cons lammie (cons new-args new-body))))

(define (exp-plambda x r mr)
  ; same as lambda except binding-lexical -> binding-pattern
  ...)

(define (exp-define x r mr)
  ; name = syntax-cadr x
  ; add_binding r name undef
  ; definition = exp syntax-caddr x r mr
  ; set! r.name definition
  (letrec* ([name (syntax-car (syntax-cdr x))]
	    [b (make-binding (syntax-car (syntax-cdr x))
			     (binding-lexical)
			     (binding-mutable)
			     (undef))])
	   (env-add-binding! b)
	   (binding-set! b (exp (syntax-car (syntax-cdr (syntax-cdr x)))
				r
				mr))))

(define (exp-define-syntax x r mr)
  ...)

(define (exp-letrec* x r mr)
  ...)

(define (syntax-null? x)
  (null? (syntax-object-expr x)))

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

(define (strip x)
  (if (syntax-object? x)
      (if (top-marked? (syntax-object-wrap x))
	  (syntax-object-expr x)
	  (strip (syntax-object-expr x)))
      (if (pair? x)
	  (letrec* ([a (strip (car x))]
		    [d (strip (cdr x))])
		   (if (if (eq? a (car x)) (eq? d (cdr x)) #f)
		       x
		       (cons a d)))
	  ; XXX handle vector too
	  x)))

(define syntax->datum strip)

; The sne cache is a list of ((env1 . bindings) . (substset . env2)),
; where env1 is the eval-time env and env2 is the expand-time env.

(define *sne-cache* '())

(define (snec-lookup env)
  (assp (lambda (a) (eq? (car a) env))
	*sne-cache*))

(define (snec-update! env bindings sne)
  (letrec* ([a (snec-lookup env)])
	   (if a
	       (begin
		 (set-cdr! (car a) bindings)
		 (set-cdr! a sne))
	       (set! *sne-cache*
		     (list (cons (cons env (env-bindings env)) sne))))))
	       

(define (initial-wrap-and-env env)
  (define specials
    (map
     (lambda (a)
       (cons (eval (car a) *root-environment*)
	     (eval (cdr a) *root-environment*)))
     '((quote . exp-quote)
       (syntax . exp-syntax)
       (lambda . exp-lambda)
       (plambda . exp-plambda)
;       (define . exp-define)
       (define-syntax . exp-define-syntax)
;       (letrec* . exp-letrec*)
       )))
  (define (subst-one sym label)
    (make-subst sym (list top-mark) label))
  (define (bind-one binding label)
    (letrec*
     ([sym (binding-name binding)]
      [value (binding-value binding)]
      [sb (assq value specials)])
     (if sb
	 (make-binding label (binding-core) (binding-immutable) (cdr sb))
	 (make-binding label (binding-lexical) (binding-immutable) sym))))
  (define (make-one-sne binding)
    (define label (make-label))
    (cons (subst-one (binding-name binding) label)
	  (bind-one binding label)))
  (define (make-frame-sne bindings cached parent-substset)
    (if (null? bindings)
	(list parent-substset '())
	(if (if cached (eq? bindings (cdr (car cached))) #f)
	    (cdr cached)
	    (letrec* ([first-sne (make-one-sne (car bindings))]
		      [rest-snes (make-frame-sne (cdr bindings)
						 cached
						 parent-substset)])
		     (cons
		      (cons (car first-sne) (car rest-snes))
		      (cons (cdr first-sne) (cdr rest-snes)))))))
  (define (parent-ss env)
    (letrec* ([parent (env-parent env)])
	     (if (null? parent)
		 '()
		 (cdr (cdr (make-env-sne parent))))))
  (define (make-env-sne env)
    (letrec* ([cached (snec-lookup env)]
	      [bindings (env-bindings env)])
	     (if (if cached (eq? (cdr (car cached)) bindings) #f)
		 (cdr cached)
		 (letrec* ([new-wne (make-frame-sne bindings
						    cached
						    (parent-ss env))])
			  (snec-update! env (env-bindings env) new-wne)
			  new-wne))))
  (define (initial-sne env)
    (if (null? env)
	'(() ())
	(letrec* ([first-ewne (make-env-sne env)]
		  [rest-ewne (initial-sne (env-parent env))])
		 (cons (car first-ewne)
		       (cons (cdr first-ewne) (cdr rest-ewne))))))
  (letrec* ([sne (initial-sne env)]
	    [ss (car sne)]
	    [ee (cdr sne)]
	    [wrap (make-wrap (make-markset top-mark)
			     ss)])
	   (cons wrap ee)))
	   
(define (expandXXX form env)
  (letrec* ([wne (initial-wrap-and-env env)]
	    [wrap (car wne)]
	    [meta-env (cdr wne)])
	   (syntax->datum
	    (exp (make-syntax-object form wrap) meta-env meta-env))))

(define (xp x)
  (trace 'expand x '=>
	 (expandXXX x *root-environment*))
  (newline))

(define (xpe x)
  (letrec* ([v (trace 'expand x '=>
		      (expandXXX x *root-environment*))])
	   (trace 'eval v '=> (eval v *root-environment*))
	   (newline)))

(xpe '123)
(xpe 'list)
(xpe '(list 1 2 3))
(xpe '(if 1 2 3))
(xpe '(quote abc))
(xp '(syntax 123))
(xpe '(lambda (a b) (cons b a)))
(xpe '((lambda (a b) (cons b a)) 1 2))
;(exit)

#|
; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ;
; ; ; Standard syntax

(define-syntax syntax-case
  (lambda (x)
    ???))

(define-syntax quasiquote
  ; see TR355 pp. 25-26.
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

(define (bound? env name)
  (define (_ bindings)
    (if (null? bindings)
	#f
	(if (eq? name (binding-name (car bindings)))
	    (car bindings)
	    (_ (cdr bindings)))))
  (_ (env-bindings env)))

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

(define (define-library namespec libraries symbols)
  (define env
    (make-environment '()))
  (define (import-binding binding)
    (env-add-binding! env
		      (make-binding
		       (binding-name binding)
		       (binding-type binding)
		       (binding-immutable)
		       (binding-value binding))))
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
	       (define binding (get-binding sym *root-environment*))
	       (env-add-binding! env
				 (make-binding sym
					       (binding-type binding)
					       (binding-mutable)
					       (binding-value binding)))
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
