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

; trace		----------------------------------

(define (last list)
  (if (null? list)
      '()
      (if (pair? list)
	  (if (null? (cdr list))
	      (car list)
	      (last (cdr list)))
	  list)))

(define (writeln exp)			; Oh, Pascal!
  (write exp)
  (newline))

(define (trace . exps)
  (writeln exps)
  (last exps))

(define (notrace . exps)
  (last exps))

; binding	----------------------------------

(define (binding-immutable) 0)
(define (binding-mutable) 1)
(define (binding-lexical) 1)
(define (binding-pattern) 3)

(define (pattern-binding? x)
  (and (binding? x) (eqv? (binding-type x) (binding-pattern))))

(define (make-binding name type mutability value)
  (assert (memv type (list (binding-lexical) (binding-pattern))))
  (assert (memv mutability (list (binding-immutable) (binding-mutable))))
  (vector 'binding name type mutability value))

(define (binding? binding)
  (and (vector? binding)
       (> (vector-length binding) 0)
       (eq? (vector-ref binding 0) 'binding)))

(define (binding-name binding)
  (notrace 'binding-name binding)
  (assert (binding? binding))
  (vector-ref binding 1))

(define (binding-type binding)
  (notrace 'binding-type binding)
  (assert (binding? binding))
  (vector-ref binding 2))

(define (binding-mutability binding)
  (notrace 'binding-mutability binding)
  (assert (binding? binding))
  (vector-ref binding 3))

(define (binding-value binding)
  (notrace 'binding-value binding)
  (assert (binding? binding))
  (vector-ref binding 4))

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
  (notrace 'sv->sl x)
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
  (notrace 'free-identifier=? id1 id2)
  (assert (identifier? id1))
  (assert (identifier? id2))
  (eq? (syntax-object-expr id1) (syntax-object-expr id2)))

(define (memfree-id=? id a)
  (notrace 'memfree-id=? id a)
  (if (syntax-null? a)
      #f
      (if (free-identifier=? id (syntax-car a))
	  a
	  (memfree-id=? id (syntax-cdr a)))))

; environment	-----------------------------

(define (make-environment parent)
  (cons '() parent))

(define env-bindings car)
(define env-parent cdr)
(define env-set-bindings! set-car!)

(define (push-environment bindings env)
  (cons bindings env))

(define (env-add-binding! env binding)
  (env-set-bindings! env (cons binding (env-bindings env))))

(define (env-lookup env name)
  (notrace 'env-lookup env name '=>
  (define (_ seg env)
    (if (null? seg)
	(if (null? env)
	    #f
	    (_ (car env) (cdr env)))
	(let ([binding (car seg)])
	  (if (eq? (binding-name binding) name)
	      binding
		     (_ (cdr seg) env)))))
  (_ '() env)))

(define (ext-env name value env)
  (cons (make-binding name (binding-pattern) (binding-immutable) value)
	env))

; * ; * ; * ; * ; * ; * ; * ; * ; * ; * ; * ; * ; * ; * ; * ; * ; * ; * ; * ;

(define (merge-envs head-env tail-env)
  (notrace 'merge-envs head-env tail-env)
  (if (null? head-env)
      tail-env
      (let* ([newn (binding-name (car head-env))]
	     [level (car (binding-value (car head-env)))]
	     [newv (cdr (binding-value (car head-env)))]
	     [oldn (binding-name (car tail-env))]
	     [oldv (cdr (binding-value (car tail-env)))]
	     [mrgv (cons newv oldv)]
	     [mrga (make-binding newn
				 (binding-pattern)
				 (binding-immutable)
				 (cons level mrgv))])
	(notrace 'merge-envs newn oldn)
	(assert (eq? newn oldn))
	(cons mrga (merge-envs (cdr head-env) (cdr tail-env))))))

(define (top-wrap x)
  (make-syntax-object x
		      (make-wrap (make-markset top-mark) (make-substset))))
(define-syntax _
  (lambda (x)
    (raise (make-syntax-error "wildcard illegal as expression"))))
  
(define-syntax ...
  (lambda (x)
    (raise (make-syntax-error "ellipsis illegal as expression"))))
  
(define wildcard (top-wrap '_))
(define ellipsis (top-wrap '...))

(define (match pattern form literals)

  (define (pattern-var-initial-bindings pattern level)
    (define (_ pattern level r)
      (cond
       [(identifier? pattern)
	(let ([sym (syntax-object-expr pattern)])
	  (cond
	   [(free-identifier=? pattern wildcard) r]
	   [(free-identifier=? pattern ellipsis) r]
	   [(memfree-id=? pattern literals) r]
	   [else (cons (make-binding sym
				     (binding-pattern)
				     (binding-immutable)
				     (cons level '()))
		       r)]))]
       [(syntax-pair? pattern)
	(if (and (pair? (syntax-cdr pattern))
		 (free-identifier=? (syntax-cadr pattern) ellipsis))
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
    (notrace 'match-ellipsis
	     (sox head-pat)
	     (sox tail-pat)
	     (sox form)
	     'env
	     level)
    (notrace 'match-ellipsis
	     (sox head-pat)
	     (sox tail-pat)
	     (sox form)
	     'env
	     level
	     '=>
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
       [(free-identifier=? pattern wildcard) env]
       [(free-identifier=? pattern ellipsis) #f]
       [(memfree-id=? pattern literals)
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
  (_ pattern form '() 0 #t))

(define (syntax->datum syntax-object)
  (notrace 's->d syntax-object)
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
		    syntax-object
		    (list->vector s)))
	      syntax-object))))

(define (datum->syntax template-id datum)
  (make-syntax-object datum (syntax-object-wrap template-id)))

(define (null-wrap x)
  (make-syntax-object x
		      (make-wrap (make-markset top-mark) (make-substset))))

(define-syntax pattern-bindings
  (syntax-rules '()
    [(_ (var level value) . rest)
     (cons
      (make-binding 'var
		    (binding-pattern)
		    (binding-immutable)
		    '(level . value))
      (pattern-bindings . rest))]
    [(_) '()]))

(define (bindings-short-names bs)
  (if (null? bs)
      '()
      (cons (let* ([b (car bs)]
		   [v (binding-value b)])
	      (list (binding-name b) (car v) (cdr v)))
	    (bindings-short-names (cdr bs)))))
    

(define-syntax test-match
  (syntax-rules '(=>)
    [(_ pattern form => expected ...)
     (begin
       (notrace 'match 'pattern 'form '=>? 'expected ...)
       (let ([actual (syntax->datum (match (null-wrap 'pattern)
					   (null-wrap 'form)
					   (null-wrap '(k))))])
	 (apply notrace 'match 'pattern 'form '=> (bindings-short-names actual))
	 (unless (equal? actual (pattern-bindings expected ...))
		 (trace 'match 'pattern 'form '=> actual)
		 (assert (equal? actual
				 (pattern-bindings expected ...))))))]))

(test-match (_ a k b)   (m 3 k 4) => (a 0 3) (b 0 4))
(test-match (_ a ...)   (m x y z) => (a 1 (x y z)))
(test-match (_ a ... b) (m x y z) => (a 1 (x y)) (b 0 z))
(test-match (_ (x y ...) ...)
	    (m (a) (b c) (d e f))
	    =>
	    (x 1 (a b d)) (y 2 (() (c) (e f))))
(test-match (_ (x ... y) ...)
	    (m (a) (b c) (d e f))
	    =>
	    (x 2 (() (b) (d e))) (y 1 (a c f)))
(test-match (_ x ... y z)
	    (m a b c d)
	    =>
	    (x 1 (a b)) (y 0 c) (z 0 d))

; * ; * ; * ; * ; * ; * ; * ; * ; * ; * ; * ; * ; * ; * ; * ; * ; * ; * ; * ;

(define (XXeval expr env)
  (notrace 'eval expr env '=>
	   (cond
	    [(eq? expr #t) #t]
	    [else (eval expr (null-environment 5))])))

(define (exp-syntax-case x r mr)
  (notrace 'exp-syntax-case (sox x) 'r 'mr)
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
		 [e (match pattern expr literals)]
		 [ee (push-environment e mr)])
	    (if (and e (XXeval fender ee))
		; XXX strip syntax from result.
		(XXeval (syntax-object-expr out-expr) ee)
		(_ (syntax-cdr clauses))))))
    (_ clauses)))

(define b (make-binding 'x
			(binding-lexical)
			(binding-mutable)
			(null-wrap '(foo))))
(set! e (make-environment '()))
(env-add-binding! e b)
#;(writeln (exp-syntax-case (null-wrap '(syntax-case x (l i t) ((_) "foo")))
			  e
			  '()))
(assert (equal? (exp-syntax-case
		 (null-wrap '(syntax-case x (l i t) ((_) "foo")))
		 e
		 '())
		"foo"))

; * ; * ; * ; * ; * ; * ; * ; * ; * ; * ; * ; * ; * ; * ; * ; * ; * ; * ; * ;

(define (car-ellipsis? x)
  (and (syntax-pair? x)
       (identifier? (syntax-car x))
       (free-identifier=? (syntax-car x) ellipsis)))

(define (cadr-ellipsis? form)
  (and (syntax-pair? form)
       (syntax-pair? (syntax-cdr form))
       (identifier? (syntax-cadr form))
       (free-identifier=? (syntax-cadr form) ellipsis)))

(assert      (cadr-ellipsis? (null-wrap '(a ... b))))
(assert (not (cadr-ellipsis? (null-wrap '(a . b)))))

(define (multi-ref list indices)
  (if (null? indices)
      list
      (multi-ref (list-ref list (car indices)) (cdr indices))))

(assert (equal? 'c (multi-ref '((a b) (c d)) '(1 0))))

(define (sub-binding binding pos)
  (assert (>= (car (binding-value binding)) (length pos)))
  (if (>= (car (binding-value binding)) (length pos))
      (multi-ref (cdr (binding-value binding)) (reverse pos))
      #f))

(define (sub-binding binding pos)
  (let* ([val (binding-value binding)]
	 [vdepth (car val)]
	 [vvals (cdr val)])
    (cond
     [(zero? vdepth) vvals]
     [(>= vdepth (length pos)) (multi-ref vvals (reverse pos))]
     [else (multi-ref vvals (list-tail (reverse-pos) vdepth))])))

(define-syntax pat
  (syntax-rules ()
      [(_ name value)
       (make-binding 'name (binding-pattern) (binding-immutable) 'value)]))
(define-syntax lex
  (syntax-rules ()
      [(_ value)
       (make-binding 'lex (binding-lexical) (binding-mutable) 'value)]))

(assert (equal? '(a b) (sub-binding (pat x (1 . (a b)))         '())))
(assert (equal? 'b     (sub-binding (pat x (1 . (a b)))         '(1))))
(assert (equal? 'c     (sub-binding (pat x (2 . ((a b) (c d)))) '(0 1))))
(assert (equal? 'p     (sub-binding (pat pat (0 . p))           '(0))))

(define (make-syntax-violation message)
  (display message)
  (newline)
  &syntax)

(define (combine-counts m n)
  (cond
   [(and m n)
    (if (eqv? m n)
	m
	(raise (make-syntax-violation "variables don't match ellipses")))]
   [m m]
   [n n]
   [else (raise (make-syntax-violation "too many ellipses"))]))

(define (repeat-count tmpl pos mr)
  (notrace 'repeat-count (syntax->datum tmpl) pos)
  (cond
   [(identifier? tmpl)
    (let ([binding (env-lookup mr (syntax-object-expr tmpl))])
      (if (and (pattern-binding? binding)
	       (> (car (binding-value binding)) (length pos)))
	  (length (sub-binding binding pos))
	  #f))]
   [(car-ellipsis? tmpl) #f]
   [(cadr-ellipsis? tmpl)
    (combine-counts
     (repeat-count (syntax-car tmpl) pos mr)
     (repeat-count (syntax-cddr tmpl) pos mr))]
   [(pair? tmpl)
    (combine-counts
     (repeat-count (car tmpl) pos mr)
     (repeat-count (cdr tmpl) pos mr))]
   [else #f]))

(define e (make-environment '()))
(env-add-binding! e (lex 42))
(env-add-binding! e (pat pat (0 . p)))
(env-add-binding! e (pat p3 (1 . (a b c))))
(env-add-binding! e (pat p3x (2 . (() (a) (a b)))))

(assert (eqv? #f (repeat-count (null-wrap '())  '(1)   e)))
(assert (eqv? #f (repeat-count (null-wrap '0)   '(1)   e)))
(assert (eqv? #f (repeat-count (null-wrap 'x)   '(1)   e)))
(assert (eqv? #f (repeat-count (null-wrap 'lex) '(1)   e)))
(assert (eqv? #f (repeat-count (null-wrap 'pat) '()    e)))
(assert (eqv? #f (repeat-count (null-wrap 'pat) '(1)   e)))
(assert (eqv?  3 (repeat-count (null-wrap 'p3)  '()    e)))
(assert (eqv? #f (repeat-count (null-wrap 'p3)  '(0)   e)))

(assert (eqv?  3 (repeat-count (null-wrap 'p3x) '()    e)))
(assert (eqv?  0 (repeat-count (null-wrap 'p3x) '(0)   e)))
(assert (eqv?  1 (repeat-count (null-wrap 'p3x) '(1)   e)))
(assert (eqv?  2 (repeat-count (null-wrap 'p3x) '(2)   e)))
(assert (eqv? #f (repeat-count (null-wrap 'p3x) '(0 0) e)))

(define (pattern-variable? x mr)
  (notrace 'pattern-variable? x 'mr)
  (notrace '-- 'env-lookup (env-lookup mr (syntax-object-expr x)))
  (and (identifier? x)
       (pattern-binding? (env-lookup mr (syntax-object-expr x)))))

(define (expand tmpl pos depth ellipsis-ok? mr)
  (define (_ tmpl pos ellipsis-ok?)
    (notrace 'expand (syntax-object-expr tmpl) pos ellipsis-ok?)
    (cond
     [(identifier? tmpl)
      (cond
       [(pattern-variable? tmpl mr)
	(let ([binding (env-lookup mr (syntax-object-expr tmpl))])
	  (if (> (car (binding-value binding)) (length pos))
	      (raise (make-syntax-violation "not enough ellipses")))
	  (datum->syntax tmpl (sub-binding binding pos)))]
       [else tmpl])]
     [(and ellipsis-ok? (car-ellipsis? tmpl)) ; (... tmpl)
      (let ([expr (syntax-object-expr tmpl)])
	(unless (and (pair? (cdr expr)) (null? (cddr expr)))
		(raise (make-syntax-violation "misplaced ellipsis"))))
      (_ (syntax-cadr tmpl) pos #f)]
     [(and ellipsis-ok? (cadr-ellipsis? tmpl)) ; (subtmpl ... . rest)
      (let loop ([count (- (repeat-count (syntax-car tmpl) pos mr) 1)]
		 [rest (_ (syntax-cddr tmpl) pos #t)])
	(if (< count 0)
	    rest
	    (loop (- count 1)
		  (cons (_ (syntax-car tmpl) (cons count pos) #t) rest))))]
     [(syntax-pair? tmpl)
      (cons (_ (syntax-car tmpl) pos ellipsis-ok?)
	    (_ (syntax-cdr tmpl) pos ellipsis-ok?))]
     [(syntax-vector? tmpl) 
      (syntax-list->syntax-vector
       (_ (syntax-vector->syntax-list tmpl) pos ellipsis-ok?))]
     [(syntax-null? tmpl) '()]
     [else tmpl]))
  (_ tmpl '() #t))

(define-syntax test-expand
  (syntax-rules '(=>)
    [(_ input => expected)
     (begin
       (notrace 'test-expand 'input '=>? 'expected)
       (let ([actual (syntax->datum (expand (null-wrap 'input) '() 0 #t e))])
	 (notrace 'expand 'input '=> actual)
	 (unless (equal? actual 'expected)
		 (trace 'expand 'input '=> actual)
		 (assert (equal? actual 'expected)))))]))

(test-expand lex                 => lex)
(test-expand pat                 => p)
(test-expand (lex . pat)         => (lex . p))
(test-expand (p3 ...)            => (a b c))
(test-expand (p3 ... pat)        => (a b c p))
(test-expand (p3 ... p3 ... pat) => (a b c a b c p))
(test-expand ((p3x ... pat) ...) => ((p) (a p) (a b p)))
(test-expand (... ...)           => ...)
(test-expand (x y (... ...))     => (x y ...))
(test-expand (... (pat ...))     => (p ...))

(define (exp-syntax x r mr)
  (expand x '() 0 #t mr))

(exit)
