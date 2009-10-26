; Chicken help

(define free-identifier=? eq?)		; close enough

(define (memp p a)
  (cond
   [(null? a) #f]
   [(p (car a)) a]
   [else (memp p (cdr a))]))

(define (make-binding name type mutability value)
  (cons name value))

(define (binding-immutable) 1)
(define (binding-pattern) 3)

(define syntax-pair? pair?)
(define syntax-car car)
(define syntax-cdr cdr)
(define syntax-vector->syntax-list vector->list)
#;(define (syntax-vector->syntax-list x)
  (extend-wrap
   (syntax-object-wrap x)
   (vector->list (syntax-object-expr x))))


; * ; * ; * ; * ; * ; * ; * ; * ; * ; * ; * ; * ; * ; * ; * ; * ; * ; * ; * ;

(define (memfree-ident= i a)
  (memp (lambda (x) (free-identifier=? i x))
	a))

(define (ext-env name value env)
  ;(cons (cons name value) env))
  (cons (make-binding name (binding-pattern) (binding-immutable) value)
	env))

(define (merge-envs head-env tail-env)
  (if (null? head-env)
      tail-env
      (let* ([newn (caar head-env)]
	     [newl (cadar head-env)]
	     [newv (cddar head-env)]
	     [oldn (caar tail-env)]
	     [oldl (cadar tail-env)]
	     [oldv (cddar tail-env)]
	     [mrgv (cons newv oldv)]
	     [mrga (cons newn (cons newl mrgv))])
	(assert (eq? newn oldn))
	(assert (eqv? newl oldl))
	(cons mrga (merge-envs (cdr head-env) (cdr tail-env))))))

#;(write (merge-envs '((a 0 . x) (b 0 . y))
		     '((a 0 2 3) (b 0 3 4) (c 0 . 5))))
#;(newline)
(assert (equal? (merge-envs '((a 0 . x) (b 0 . y))
			    '((a 0 2 3) (b 0 3 4) (c 0 . 5)))
		'((a . (0 x 2 3)) (b . (0 y 3 4)) (c 0 . 5))))

(define (match form pattern literals env)

  (define (pattern-var-initial-bindings pattern level)
    (define (_ pattern level r)
      (cond
       [(symbol? pattern)
	(cond
	 [(free-identifier=? pattern '_) r]
	 [(free-identifier=? pattern '...) r]
	 [(memfree-ident= pattern literals) r]
	 [else (cons (cons pattern (cons level '())) r)])]
       [(pair? pattern)
	(if (and (pair? (cdr pattern)) (free-identifier=? (cadr pattern) '...))
	    (_ (car pattern) (+ level 1) (_ (cddr pattern) level r))
	    (_ (car pattern) level (_ (cdr pattern) level r)))]
       [else r]))
    (_ pattern level '()))

  (define (match-ellipsis form head-pat tail-pat env level)
    (define (prepend-head-vars tail-vars)
      (append
       (pattern-var-initial-bindings head-pat (+ level 1))
       tail-vars))
    (cond
     [(_ form tail-pat env level #f) => prepend-head-vars]
     [(syntax-pair? form)
      (let ([tenv
	     (match-ellipsis (syntax-cdr form) head-pat tail-pat env level)]
	    [henv (_ (syntax-car form) head-pat '() (+ level 1) #t)])
	(merge-envs henv tenv))]
     [else #f]))
	    
  (define (_ form pattern env level ellipsis-ok)
    (cond
     [(symbol? pattern)
      (cond
       [(free-identifier=? pattern '_) env]
       [(free-identifier=? pattern '...) #f]
       [(memfree-ident= pattern literals)
	(and (free-identifier=? form pattern) env)]
       [else (ext-env pattern (cons level form) env)])]
     [(pair? pattern)
      (cond
       [(and ellipsis-ok
	     (pair? (cdr pattern))
	     (free-identifier=? (cadr pattern) '...))
	(match-ellipsis form (car pattern) (cddr pattern) env level)]
       [(syntax-pair? form)
	(let ([e (_ (syntax-cdr form) (cdr pattern) env level ellipsis-ok)])
	  (and e (_ (syntax-car form) (car pattern) e level ellipsis-ok)))]
       [else #f])]
     [(and (vector? pattern) (vector? form))
      (_ (syntax-vector->syntax-list form)
	 (vector->list pattern)
	 env
	 level
	 ellipsis-ok)]
     [else (and (equal? form pattern) env)]))

  (_ form pattern env 0 #t))

;(write (match '(m 3 k 4) '(_ a k b) '(k) '(boo)))
;(newline)
(assert (equal? (match '(m 3 k 4) '(_ a k b) '(k) '(boo))
		'((a . (0 . 3)) (b . (0 . 4)) boo)))

;(write (match '(m x y z) '(_ a ...) '() '((boo . (0 #t)))))
;(newline)
(assert (equal? (match '(m x y z) '(_ a ...) '() '((boo . (0 . #t))))
		'((a . (1 . (x y z))) (boo . (0 . #t)))))

;(write (match '(m x y z) '(_ a ...) '() 'boo))
;(newline)
(assert (equal? (match '(m x y z) '(_ a ...) '() 'boo)
		'((a . (1 . (x y z))) . boo)))

;(write (match '(m x y z w) '(_ a ... b) '() '((boo . (0 . #t)))))
;(newline)
(assert (equal? (match '(m x y z w) '(_ a ... b) '() '((boo . (0 . #t))))
		'((a . (1 . (x y z))) (b . (0 . w)) (boo . (0 . #t)))))

#;(write (match '(m k (a) (b c) (d e f))
	      '(_ k (x y ...) ...)
	      '(k)
	      '(boo)))
#;(newline)
(assert (equal? (match '(m k (a) (b c) (d e f))
		       '(_ k (x y ...) ...)
		       '(k)
		       '(boo))
		'((x 1 a b d) (y 2 () (c) (e f)) boo)))
	
;(write (match '#(m a b c) '#(_ x ... z) '() '(boo)))
;(newline)
(assert (equal? (match '#(m a b c) '#(_ x ... z) '() '(boo))
		'((x . (1 . (a b))) (z . (0 . c)) boo)))

(define (find-rule form rules name literals)
  (cond
   [(null? rules)
    (error "syntax violation" name form)]
   [else (let ([e (match form (caar rules) literals '())])
	   (if e (list (caar rules) (cadar rules) e)
	       (find-rule form (cdr rules) name literals)))]))
    
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
