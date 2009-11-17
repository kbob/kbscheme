; Chicken help

;(define free-identifier=? eq?)		; close enough

(define (memp p a)
  (cond
   [(null? a) #f]
   [(p (car a)) a]
   [else (memp p (cdr a))]))

(define (remp proc list)
  (define (_ list result)
    (if (null? list)
	(reverse result)
	(_ (cdr list)
	   (if (proc (car list))
	       result
	       (cons (car list) result)))))
  (_ list '()))

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

(define (make-binding name type mutability value)
  (cons name value))

(define (binding-immutable) 0)
(define (binding-mutable) 1)
(define (binding-lexical) 1)
(define (binding-pattern) 3)

; trace		----------------------------------

(define (last list)
  (if (null? list)
      '()
      (if (pair? list)
	  (if (null? (cdr list))
	      (car list)
	      (last (cdr list)))
	  list)))

(define (trace . exps)
  (write exps)
  (newline)
  (last exps))

(define (notrace . exps)
  (last exps))

; tagged-vector	 ---------------------------------

(define (tagged-vector? tag obj)
  (and (vector? obj)
       (> (vector-length obj) 0)
       (eq? (vector-ref obj 0) tag)))

; label		----------------------------------

(define *lc* 0)

(define (make-label)
  (set! *lc* (+ *lc* 1))
  (vector 'label *lc*))

(define (label? obj)
  (tagged-vector? 'label obj))

; mark		----------------------------------

(define *mc* 0)

(define (make-mark)
  (set! *mc* (+ *mc* 1))
  (vector 'mark *mc*))

(define (mark? obj)
  (tagged-vector? 'mark obj))

; markset	----------------------------------

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
  (vector 'subst sym markset label))

(define (subst? obj)
  (tagged-vector? 'subst obj))

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
  (vector 'wrap markset substset))

(define (wrap? obj)
  (tagged-vector? 'wrap obj))

(define (wrap-markset wrap)
  (vector-ref wrap 1))

(define (wrap-substset wrap)
  (vector-ref wrap 2))

(define (join-wraps w1 w2)
  (make-wrap (join-marksets (wrap-markset w1) (wrap-markset w2))
             (join-substsets (wrap-substset w1) (wrap-substset w2))))

; ???		 ---------------------------------

(define top-mark (make-mark))

(define (top-marked? wrap)
  (markset-has-mark? top-mark (wrap-markset wrap)))

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

#;(define (id-label id)
  (notrace 'id-label (syntax-object-expr id) '=>
  (letrec* ([sym (syntax-object-expr id)]
	    [wrap (syntax-object-wrap id)]
	    [markset (wrap-markset wrap)]
	    [substset (wrap-substset wrap)]
	    [sub (substset-find substset sym markset)])
	   (if sub
	       (subst-label sub)
	       (syntax-error id
			     (string-append
			      "undefined identifier: "
			      (symbol->string (syntax-object-expr id)))))))
  )

; syntax-object	-----------------------------

(define (make-syntax-object expr wrap)
  (vector 'syntax-object expr wrap))

(define (syntax-object? obj)
  (tagged-vector? 'syntax-object obj))

(define (syntax-object-expr stx)
  (assert (syntax-object? stx))
  (vector-ref stx 1))
(define sox syntax-object-expr)

(define (syntax-object-wrap stx)
  (assert (syntax-object? stx))
  (vector-ref stx 2))

(define (syntax-vector? obj)
  (and (syntax-object? obj)
       (vector? (syntax-object-expr obj))))

; syntax-pair	-----------------------------

(define (syntax-pair? obj)
  (and (syntax-object? obj) (pair? (syntax-object-expr obj))))

(define (syntax-null? obj)
  (and (syntax-object? obj) (null? (syntax-object-expr obj))))

(define (syntax-car x)
  (extend-wrap
   (syntax-object-wrap x)
   (car (syntax-object-expr x))))

(define (syntax-cdr x)
  (extend-wrap
   (syntax-object-wrap x)
   (cdr (syntax-object-expr x))))

(define (syntax-cadr x)
  (syntax-car (syntax-cdr x)))

(define (syntax-cddr x)
  (syntax-cdr (syntax-cdr x)))

(define (syntax-caddr x)
  (syntax-car (syntax-cdr (syntax-cdr x))))

(define (syntax-cdddr x)
  (syntax-cdr (syntax-cdr (syntax-cdr x))))

(define (syntax-vector->syntax-list x)
  (extend-wrap
   (syntax-object-wrap x)
   (vector->list (syntax-object-expr x))))

;(define syntax-pair? pair?)
;(define syntax-car car)
;(define syntax-cdr cdr)
;(define syntax-cadr cadr)
;(define syntax-cddr cddr)
;(define syntax-caddr caddr)
;(define syntax-cdddr cdddr)
;(define syntax-vector->syntax-list vector->list)

; identifier	-----------------------------

(define (identifier? obj)
  (and (syntax-object? obj)
       (symbol? (syntax-object-expr obj))))

(define (free-identifier=? id1 id2)
  (assert (identifier? id1))
  (assert (identifier? id2))
  (eq? (syntax-object-expr id1) (syntax-object-expr id2)))

#;(free-identifier=? (make-syntax-object 'a #f) (make-syntax-object 'b #f))

; environment	-----------------------------

(define (make-environment parent)
  (cons '() parent))

(define env-bindings car)
(define env-parent cdr)
(define env-set-bindings! set-car!)

(define (push-environment bindings env)
  (cons bindings env))

(define (binding-name binding)
  (car binding))

(define (binding-value binding)
  (cdr binding))

(define (env-add-binding! env binding)
  (env-set-bindings! env (cons binding (env-bindings env))))

(define (env-lookup env name)
  (define (_ seg env)
    (if (null? seg)
	(if (null? env)
	    #f
	    (_ (car env) (cdr env)))
	(let ([binding (car seg)])
	  (if (eq? (binding-name binding) name)
	      binding
		     (_ (cdr seg) env)))))
  (_ '() env))

(define (ext-env name value env)
  (cons (make-binding name (binding-pattern) (binding-immutable) value)
	env))

; * ; * ; * ; * ; * ; * ; * ; * ; * ; * ; * ; * ; * ; * ; * ; * ; * ; * ; * ;

(define (merge-envs head-env tail-env)
  (notrace 'merge-envs head-env tail-env)
  (if (null? head-env)
      tail-env
      (let* ([newn (caar head-env)]
	     [level (cadar head-env)]
	     [newv (cddar head-env)]
	     [oldn (caar tail-env)]
	     [oldv (cddar tail-env)]
	     [mrgv (cons newv oldv)]
	     [mrga (cons newn (cons level mrgv))])
	(notrace 'merge-envs newn oldn)
	(assert (eq? newn oldn))
	(cons mrga (merge-envs (cdr head-env) (cdr tail-env))))))

#;(write (merge-envs '((a 0 . x) (b 0 . y))
		     '((a 0 2 3) (b 0 3 4) (c 0 . 5))))
#;(newline)
#;(assert (equal? (merge-envs '((a 0 . x) (b 0 . y))
			      '((a 0 2 3) (b 0 3 4) (c 0 . 5)))
		  '((a . (0 x 2 3)) (b . (0 y 3 4)) (c 0 . 5))))

(define (match pattern form literals env)

  (define (pattern-var-initial-bindings pattern level)
    (define (_ pattern level r)
      (cond
       [(identifier? pattern)
	(cond
	 [(eq? (syntax-object-expr pattern) '_) r]
	 [(eq? (syntax-object-expr pattern) '...) r]
	 [(memq (syntax-object-expr pattern) literals) r]
	 [else (cons (cons (syntax-object-expr pattern) (cons level '())) r)])]
       [(syntax-pair? pattern)
	(if (and (pair? (syntax-cdr pattern))
		 (free-identifier=? (syntax-cadr pattern) '...))
	    (_ (syntax-car pattern)
	       (+ level 1)
	       (_ (syntax-cddr pattern) level r))
	    (_ (syntax-car pattern) level (_ (syntax-cdr pattern) level r)))]
       [else r]))
    (_ pattern level '()))

  (define (match-ellipsis head-pat tail-pat form env level)
    ; head-pat is the pattern before the ellipsis.
    ; tail-pat is the pattern after the ellipsis.
    ; We match tail-pat first.
    (define (prepend-head-vars tail-vars)
      (append
       (pattern-var-initial-bindings head-pat (+ level 1))
       tail-vars))
    (notrace 'match-ellipsis (sox head-pat) (sox tail-pat) (sox form) 'env level)
    (notrace 'match-ellipsis (sox head-pat) (sox tail-pat) (sox form) 'env level '=>
    (cond
     [(_ tail-pat form env level #f) => prepend-head-vars]
     [(syntax-pair? form)
      (let ([tenv
	     (match-ellipsis head-pat tail-pat (syntax-cdr form) env level)]
	    [henv (_ head-pat (syntax-car form) '() (+ level 1) #t)])
	(notrace 'tenv tenv)
	(notrace 'henv henv)
	(merge-envs henv tenv))]
     [else #f])
    )
    )

  (define (_ pattern form env level ellipsis-ok)
    (notrace 'match (sox pattern) (sox form) 'env level ellipsis-ok)
    (notrace 'match (sox pattern) (sox form) 'env level ellipsis-ok '=>
    (cond
     [(identifier? pattern)
      (notrace 'match 'identifier pattern)
      (cond
       [(eq? (syntax-object-expr pattern) '_) env]
       [(eq? (syntax-object-expr pattern) '...) #f]
       [(memq (syntax-object-expr pattern) literals)
	(and (free-identifier=? form pattern) env)]
       [else (ext-env (syntax-object-expr pattern) (cons level form) env)])]
     [(syntax-pair? pattern)
      (notrace 'match 'pair)
      (cond
       [(and ellipsis-ok
	     (syntax-pair? (syntax-cdr pattern))
	     (eq? (syntax-object-expr (syntax-cadr pattern)) '...))
	(notrace 'match 'pair 'case 1)
	(match-ellipsis (syntax-car pattern)
			(syntax-cddr pattern)
			form
			env
			level)]
       [(syntax-pair? form)
	(notrace 'match 'pair 'case 2)
	(let ([e (_ (syntax-cdr pattern)
		    (syntax-cdr form)
		    env
		    level
		    ellipsis-ok)])
	  (and e (_ (syntax-car pattern)
		    (syntax-car form)
		    e
		    level
		    ellipsis-ok)))]
       [else #f])]
     [(and (syntax-vector? pattern) (syntax-vector? form))
      (_ (syntax-vector->syntax-list pattern)
	 (syntax-vector->syntax-list form)
	 env
	 level
	 ellipsis-ok)]
     [else
      (notrace 'match 'other (sox pattern) (sox form) '=>
      (and (equal? (syntax-object-expr form)
		   (syntax-object-expr pattern))
	   env)
      )
      ])
    )
    )
  (notrace 'match (sox pattern) (sox form) literals 'env)
  (_ pattern form env 0 #t))

(define (syntax->datum syntax-object)
  (if (syntax-object? syntax-object)
      (if (top-marked? (syntax-object-wrap syntax-object))
	  (syntax-object-expr syntax-object)
	  (syntax->datum (syntax-object-expr syntax-object)))
      (if (pair? syntax-object)
	  (let ([a (syntax->datum (car syntax-object))]
		[d (syntax->datum (cdr syntax-object))])
	    (if (and (eq? a (car syntax-object)) (eq? d (cdr syntax-object)))
		syntax-object
		(cons a d)))
	  (if (vector? syntax-object)
	      (let* ([l (vector->list syntax-object)]
		     [s (syntax->datum l)])
		(if (eq? s l)
		    l
		    (list->vector s)))
	      syntax-object))))

(define (datum->syntax template-id datum)
  (make-syntax-object datum (syntax-object-wrap template)))

(define (null-wrap x)
  (make-syntax-object x
		      (make-wrap (make-markset top-mark) (make-substset))))

(write (syntax->datum (match (null-wrap '(_ a k b))
			     (null-wrap '(m 3 k 4))
			     '(k)
			     '(boo))))
(newline)
(assert (equal? (syntax->datum (match (null-wrap '(_ a k b))
				      (null-wrap '(m 3 k 4))
				      '(k)
				      '(boo)))
		'((a . (0 . 3)) (b . (0 . 4)) boo)))

(write (syntax->datum (match (null-wrap '(_ a ...))
			     (null-wrap '(m x y z))
			     '()
			     '((boo . (0 #t))))))
(newline)
(assert (equal? (syntax->datum (match (null-wrap '(_ a ...))
				      (null-wrap '(m x y z))
				      '()
				      '((boo . (0 . #t)))))
		'((a . (1 . (x y z))) (boo . (0 . #t)))))

(write (syntax->datum (match (null-wrap '(_ a ...))
			     (null-wrap '(m x y z))
			     '()
			     'boo)))
(newline)
(assert (equal? (syntax->datum (match (null-wrap '(_ a ...))
				      (null-wrap '(m x y z))
				      '()
				      '()))
		'((a . (1 . (x y z))))))

(write (syntax->datum (match (null-wrap '(_ a ... b))
			     (null-wrap '(m x y z w))
			     '()
			     '((boo . (0 . #t))))))
(newline)
(assert (equal? (syntax->datum (match (null-wrap '(_ a ... b))
				      (null-wrap '(m x y z w))
				      '()
				      '((boo . (0 . #t)))))
		'((a . (1 . (x y z))) (b . (0 . w)) (boo . (0 . #t)))))

(write (syntax->datum (match (null-wrap '(_ k (x y ...) ...))
			     (null-wrap '(m k (a) (b c) (d e f)))
			     '(k)
			     '(boo))))
(newline)
(assert (equal? (syntax->datum (match (null-wrap '(_ k (x y ...) ...))
				      (null-wrap '(m k (a) (b c) (d e f)))
				      '(k)
				      '(boo)))
		'((x 1 a b d) (y 2 () (c) (e f)) boo)))
	
(write (syntax->datum (match (null-wrap '#(_ x ... z))
			     (null-wrap '#(m a b c))
			     '()
			     '(boo))))
(newline)
(assert (equal? (syntax->datum (match (null-wrap '#(_ x ... z))
			      (null-wrap '#(m a b c))
			      '()
			      '(boo)))
		'((x . (1 . (a b))) (z . (0 . c)) boo)))

; * ; * ; * ; * ; * ; * ; * ; * ; * ; * ; * ; * ; * ; * ; * ; * ; * ; * ; * ;

(define (exp-syntax x r mr)
  walk tree.  For each symbol, if it's in mr,
  substitute its bound value.
  
)

; * ; * ; * ; * ; * ; * ; * ; * ; * ; * ; * ; * ; * ; * ; * ; * ; * ; * ; * ;

(define (XXeval expr env)
  (trace 'eval expr env '=>
	 (cond
	  [(eq? expr #t) #t]
	  [else (eval expr (null-environment 5))])))

(define (exp-syntax-case x r mr)
  (trace 'exp-syntax-case (sox x) 'r 'mr)
  (assert (syntax-object? x))
  (assert (eq? (sox (syntax-car x)) 'syntax-case))
  (let* ([expr (binding-value (env-lookup e 'x))]
	[literals (syntax-object-expr (syntax-caddr x))]
	[clauses (syntax-cdddr x)])
    (define (_ clauses)
      (if (syntax-null? clauses)
	  (syntax-error "syntax-case: no pattern matched")
	  (let* ([clause (syntax-car clauses)]
		 [pattern (syntax-car clause)]
		 [has-fender (pair? (syntax-cddr clause))]
		 [fender (if has-fender (syntax-cadr clause) '#t)]
		 [out-expr ((if has-fender syntax-caddr syntax-cadr) clause)]
		 [e (match pattern expr literals '())]
		 [ee (push-environment e mr)])
	    (trace 'Kilroy)
	    (if (and e (XXeval fender ee))
		(XXeval (syntax-object-expr out-expr) ee)
		(_ (syntax-cdr clauses))))))
    (_ clauses)))
		    

(define b (make-binding 'x
			(binding-lexical)
			(binding-mutable)
			(null-wrap '(foo))))
(set! e (make-environment '()))
(env-add-binding! e b)
(write (exp-syntax-case (null-wrap '(syntax-case x (l i t) ((_) "foo")))
			e
			'()))
(newline)
(exit)

; * ; * ; * ; * ; * ; * ; * ; * ; * ; * ; * ; * ; * ; * ; * ; * ; * ; * ; * ;
	   
(define (find-rule form rules name literals)
  (if (null? rules)
      (error "syntax violation" name form)
      (let ([rule (car rules)]
	    [has-fender (pair? (cddr rule))]
	    [fender (and has-fender (cadr rule))]
	    [out-expr ((if has-fender caddr cadr) rule)]
	    [e (syntax->datum (match (caar rules) form literals '()))])
	(if (and e (or (not has-fender) (eval fender e)))
	    (list out-expr e)
	    (find-rule form (cdr rules) name literals)))))
    
#|
(define (exp-syntax-case x)
  ; Get the rule.
  ; Make the meta-environment.
  ; Eval the action in the meta-environment.
  (let* ([literals (syntax-cadr x)]
	 [rules (syntax-cddr x)]
	 [ee (find-rule)])
    (
)
|#
(define (map-improper f a)
  (cond [(null? a) a]
	[(pair? a) (cons (f (car a)) (map-improper f (cdr a)))]
	[else (f a)]))
  
(assert (equal? (map-improper (lambda (x) (cons 'label x)) '(a b . c))
		'((label . a) (label . b) . (label . c))))

(define (subst-ellipsis var tmpl val* env)
  (map (lambda (v)
	 (tmpl->form #f tmpl (cons (cons var v) env)))
       val*))

(define (tmpl->form pattern form env)
  (cond
   [(not (pair? form))
    (let ([v (assv form env)])
      (if v (cdr v) form))]
   [(and (pair? form)
	 (pair? (cdr form))
	 (eq? (cadr form) '...))
    (let ([var (if (pair? pattern) (car pattern) pattern)])
      (let ([v-ell (assq '... env)]
	    [v-var (assq var env)])
	(if v-ell
	    (if v-var
		(append (subst-ellipsis
			 var
			 (car form)
			 (if v-var
			     (cons (cdr v-var) (cdr v-ell))
			     (cdr v-ell))
			 env)
			(cddr form))
		(append (list (tmpl->form #f (car form) env))
			(cdr v-ell)
			(cddr form)))
	    (error "unmatched ... in syntax-rules"))))]
   [(pair? form)
    (cons (tmpl->form (if (pair? pattern)
			  (car pattern)
			  #f)
		      (car form)
		      env)
	  (tmpl->form (if (pair? pattern)
			 (cdr pattern)
			 #f)
		     (cdr form)
		     env))]
   [else form]))

;(display (subst-ellipsis 'v '(k v) '(1 2 3) '((k . -))))
;(newline)
(assert (equal? (subst-ellipsis 'v '(k v) '(1 2 3) '((k . -)))
		'((- 1) (- 2) (- 3))))


(define rules
  '([(_) (foo)]
    [(_ k v ...) (foo v ...)]))
(define form '(x x x k k k))
(define pattern '(k))
(define literals '(k))
(define env '())
(exit)
