; Chicken help

(define (assert assertion)
  (if assertion
      (if #f #f)
      (raise "assertion failed:" assertion)))

(define (memp p a)
  (if (null? a)
      #f
      (if (p (car a))
          a
          (memp p (cdr a)))))

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

(define (writeln exp)                   ; Oh, Pascal!
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
  (if (binding? x) (eqv? (binding-type x) (binding-pattern)) #f))

(define (make-binding name type mutability value)
  (assert (memv type (list (binding-lexical) (binding-pattern))))
  (assert (memv mutability (list (binding-immutable) (binding-mutable))))
  (vector 'binding name type mutability value))

(define (binding? binding)
  (if (if (vector? binding)
            (> (vector-length binding) 0) #f)
       (eq? (vector-ref binding 0) 'binding) #f))

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
  (if (if (vector? obj)
            (> (vector-length obj) 0) #f)
       (eq? (vector-ref obj 0) tag) #f))

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

(define (null-wrap x)
  (make-syntax-object x
                      (make-wrap (make-markset top-mark) (make-substset))))

; ???		----------------------------------

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

; syntax-object	 ---------------------------------

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
  (if (syntax-object? obj)
       (vector? (syntax-object-expr obj)) #f))

; syntax-pair	----------------------------------

(define (syntax-pair? obj)
  (if (syntax-object? obj) (pair? (syntax-object-expr obj)) #f))

(define (syntax-null? obj)
  (if (syntax-object? obj) (null? (syntax-object-expr obj)) #f))

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

; identifier	----------------------------------

(define (identifier? obj)
  (if (syntax-object? obj)
       (symbol? (syntax-object-expr obj)) #f))

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

; environment	----------------------------------

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
        ((lambda (binding)
           (if (eq? (binding-name binding) name)
               binding
               (_ (cdr seg) env)))
         (car seg))))
  (_ '() env)))

(define (extend-frame name value env)
  (cons (make-binding name (binding-pattern) (binding-immutable) value)
        env))

; * ; * ; * ; * ; * ; * ; * ; * ; * ; * ; * ; * ; * ; * ; * ; * ; * ; * ; * ;

(define (merge-envs head-env tail-env)
  (notrace 'merge-envs head-env tail-env)
  (if (null? head-env)
      tail-env
      (begin
        (define newn (binding-name (car head-env)))
        (define level (car (binding-value (car head-env))))
        (define newv (cdr (binding-value (car head-env))))
        (define oldn (binding-name (car tail-env)))
        (define oldv (cdr (binding-value (car tail-env))))
        (define mrgv (cons newv oldv))
        (define mrga (make-binding newn
                                   (binding-pattern)
                                   (binding-immutable)
                                   (cons level mrgv)))
        (notrace 'merge-envs newn oldn)
        (assert (eq? newn oldn))
        (cons mrga (merge-envs (cdr head-env) (cdr tail-env))))))

(define (top-wrap x)
  (make-syntax-object x
                      (make-wrap (make-markset top-mark) (make-substset))))
(define-syntax _
  (lambda (x)
    (syntax-error "wildcard illegal as expression")))
  
(define-syntax ...
  (lambda (x)
    (syntax-error "ellipsis illegal as expression")))
  
(define wildcard (top-wrap '_))
(define ellipsis (top-wrap '...))

(define (car-ellipsis? x)
  (if (if (syntax-pair? x)
            (identifier? (syntax-car x)) #f)
       (free-identifier=? (syntax-car x) ellipsis) #f))

(define (cadr-ellipsis? form)
  (if (if (if (syntax-pair? form)
                 (syntax-pair? (syntax-cdr form)) #f)
            (identifier? (syntax-cadr form)) #f)
       (free-identifier=? (syntax-cadr form) ellipsis) #f))

(assert      (cadr-ellipsis? (null-wrap '(a ... b))))
(assert (not (cadr-ellipsis? (null-wrap '(a . b)))))

(define (match pattern form literals)

  (define (pattern-var-initial-bindings pattern level)
    (define (_ pattern level r)
      (if (identifier? pattern)
          ((lambda (sym)
             (if (free-identifier=? pattern wildcard)
                 r
                 (if (free-identifier=? pattern ellipsis)
                     r
                     (if (memfree-id=? pattern literals)
                         r
                         (cons (make-binding sym
                                             (binding-pattern)
                                             (binding-immutable)
                                             (cons level '()))
                               r)))))
           (syntax-object-expr pattern))
          (if (syntax-pair? pattern)
              (if (cadr-ellipsis? pattern)
                  (_ (syntax-car pattern)
                     (+ level 1)
                     (_ (syntax-cddr pattern) level r))
                  (_ (syntax-car pattern)
                     level
                     (_ (syntax-cdr pattern) level r)))
              r)))
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
             ((lambda (tp)
                (if tp
                    (prepend-head-vars tp)
                    (if (syntax-pair? form)
                        ((lambda (tenv henv)
                          (notrace 'tenv tenv)
                          (notrace 'henv henv)
                          (merge-envs henv tenv))
                         (match-ellipsis head-pat
                                         tail-pat
                                         (syntax-cdr form)
                                         env
                                         level)
                         (_ head-pat (syntax-car form) '() (+ level 1) #t))
                        #f)))
              (_ tail-pat form env level #f))))

  (define (_ pattern form env level ellipsis-ok)
    (notrace 'match (sox pattern) (sox form) 'env level ellipsis-ok)
    (notrace 'match (sox pattern) (sox form) 'env level ellipsis-ok '=>
             (if (identifier? pattern)
                 (begin
                   (notrace 'match 'identifier pattern)
                   (if (free-identifier=? pattern wildcard)
                       env
                       (if (free-identifier=? pattern ellipsis)
                           #f
                           (if (memfree-id=? pattern literals)
                               (if (free-identifier=? form pattern) env #f)
                               (extend-frame (syntax-object-expr pattern)
                                             (cons level form)
                                             env)))))
                 (if (syntax-pair? pattern)
                     (begin
                       (notrace 'match 'pair)
                       (if (if ellipsis-ok (cadr-ellipsis? pattern) #f)
                           (begin
                             (notrace 'match 'pair 'case 1)
                             (match-ellipsis (syntax-car pattern)
                                             (syntax-cddr pattern)
                                             form
                                             env
                                             level))
                           (if (syntax-pair? form)
                               (begin
                                 (notrace 'match 'pair 'case 2)
                                 ((lambda (e)
                                    (if e (_ (syntax-car pattern)
                                             (syntax-car form)
                                             e
                                             level
                                             ellipsis-ok) #f))
                                  (_ (syntax-cdr pattern)
                                     (syntax-cdr form)
                                     env
                                     level
                                     ellipsis-ok)))
                               #f)))
                     (if (if (syntax-vector? pattern) ; #(a b c)
                             (syntax-vector? form)
                             #f)
                         (_ (syntax-vector->syntax-list pattern)
                            (syntax-vector->syntax-list form)
                            env
                            level
                            ellipsis-ok)
                         (notrace 'match 'other (sox pattern) (sox form) '=>
                                  (if (eqv? (syntax-object-expr form)
                                            (syntax-object-expr pattern))
                                      env #f)))))))

  (notrace 'match (sox pattern) (sox form) literals 'env)
  (_ pattern form '() 0 #t))

(define (scons pair left right)
  (if (if (eq? left (car pair))
          (eq? right (cdr pair))
          #f)
      pair
      (cons left right)))

(define (syntax->datum syntax-object)
  (notrace 's->d syntax-object)
  (if (syntax-object? syntax-object)
      (if (top-marked? (syntax-object-wrap syntax-object))
          (syntax-object-expr syntax-object)
          (syntax->datum (syntax-object-expr syntax-object)))
      (if (pair? syntax-object)
          (scons syntax-object
                 (syntax->datum (car syntax-object))
                 (syntax->datum (cdr syntax-object)))
          (if (vector? syntax-object)
              ((lambda (l)
                 ((lambda (s)
                    (if (eq? s l)
                        syntax-object
                        (list->vector s)))
                  (syntax->datum l)))
               (vector->list syntax-object))
              syntax-object))))

(define (datum->syntax template-id datum)
  (make-syntax-object datum (syntax-object-wrap template-id)))

(define (pattern-bindings sbs)
  (define (bind-one var level value)
    (make-binding var (binding-pattern) (binding-immutable) (cons level value)))
  (if (null? sbs)
      '()
      (cons (bind-one (caar sbs) (cadar sbs) (caddar sbs))
            (pattern-bindings (cdr sbs)))))

(define (bindings-short-names bs)
  (if (null? bs)
      '()
      (cons (begin
              (define b (car bs))
              (define v (binding-value b))
              (list (binding-name b) (car v) (cdr v)))
            (bindings-short-names (cdr bs)))))
    
(define (struct-eqv? l r)
  (if (pair? l)
      (if (pair? r)
          (if (struct-eqv? (car l) (car r))
              (struct-eqv? (cdr l) (cdr r))
              #f)
          #f)
      (if (vector? l)
          (if (vector? r)
              (struct-eqv? (vector->list l) (vector->list r))
              #f)
          (eqv? l r))))

(define (test-match pattern form => . expecteds)
  (notrace 'match pattern form '=>? expecteds)
  (assert (eq? => '=>))
  (define (report actual)
    (apply trace 'match pattern form '=> (bindings-short-names actual))
    (if (not (struct-eqv? actual (pattern-bindings expecteds)))
        (begin
          (apply trace 'match pattern form '=> (bindings-short-names actual))
          (error))))
  (report (syntax->datum (match (null-wrap pattern)
                                (null-wrap form)
                                (null-wrap '(k))))))

(test-match '(_ a k b)   '(m 3 k 4) '=> '(a 0 3) '(b 0 4))
(test-match '(_ a ...)   '(m x y z) '=> '(a 1 (x y z)))
(test-match '(_ a ... b) '(m x y z) '=> '(a 1 (x y)) '(b 0 z))
(test-match '(_ (x y ...) ...)
            '(m (a) (b c) (d e f))
            '=>
            '(x 1 (a b d)) '(y 2 (() (c) (e f))))
(test-match '(_ (x ... y) ...)
            '(m (a) (b c) (d e f))
            '=>
            '(x 2 (() (b) (d e))) '(y 1 (a c f)))
(test-match '(_ x ... y z)
            '(m a b c d)
            '=>
            '(x 1 (a b)) '(y 0 c) '(z 0 d))

; * ; * ; * ; * ; * ; * ; * ; * ; * ; * ; * ; * ; * ; * ; * ; * ; * ; * ; * ;

(define (XXeval expr env)
  (notrace 'eval expr env '=>
           (if (eq? expr #t)
               #t
               (eval expr (null-environment 5)))))

(define (exp-syntax-case x r mr)
  (notrace 'exp-syntax-case (sox x) 'r 'mr)
  (assert (syntax-object? x))
  (assert (eq? (sox (syntax-car x)) 'syntax-case))
  (define expr (binding-value (env-lookup e 'x)))
  (define literals (syntax-object-expr (syntax-caddr x)))
  (define clauses (syntax-cdddr x))
  (define (_ clauses)
    (if (syntax-null? clauses)
        (syntax-error "syntax-case: no pattern matched")
        (begin
          (define clause (syntax-car clauses))
          (define pattern (syntax-car clause))
          (define has-fender (pair? (syntax-cddr clause)))
          (define fender (if has-fender (syntax-cadr clause) '#t))
          (define out-expr ((if has-fender syntax-caddr syntax-cadr) clause))
          (define eee (match pattern expr literals))
          (define ee (push-environment eee mr))
          (if (if eee (XXeval fender ee) #f)
              (XXeval (syntax-object-expr out-expr) ee)
              (_ (syntax-cdr clauses))))))
  (_ clauses))

(define b (make-binding 'x
                        (binding-lexical)
                        (binding-mutable)
                        (null-wrap '(foo))))
(define e (make-environment '()))
(env-add-binding! e b)
#;(writeln (exp-syntax-case (null-wrap '(syntax-case x (l i t) ((_) "foo")))
                          e
                          '()))
(assert (string=? (exp-syntax-case
                   (null-wrap '(syntax-case x (l i t) ((_) "foo")))
                   e
                   '())
                  "foo"))

; * ; * ; * ; * ; * ; * ; * ; * ; * ; * ; * ; * ; * ; * ; * ; * ; * ; * ; * ;

(define (multi-ref list indices)
  (if (null? indices)
      list
      (multi-ref (list-ref list (car indices)) (cdr indices))))

(assert (eq? 'c (multi-ref '((a b) (c d)) '(1 0))))

(define (sub-binding binding pos)
  ((lambda (val)
     ((lambda (vdepth)
        ((lambda (vvals)
           (if (zero? vdepth)
               vvals
               (if (>= vdepth (length pos))
                   (multi-ref vvals (reverse pos))
                   (multi-ref vvals (list-tail (reverse pos) vdepth)))))
         (cdr val)))
      (car val)))
   (binding-value binding)))

(define (pat name value)
  (make-binding name (binding-pattern) (binding-immutable) value))

(define (lex value)
  (make-binding 'lex (binding-lexical) (binding-mutable) value))

(assert (struct-eqv? '(a b) (sub-binding (pat 'x '(1 . (a b)))         '())))
(assert (struct-eqv? 'b     (sub-binding (pat 'x '(1 . (a b)))         '(1))))
(assert (struct-eqv? 'c     (sub-binding (pat 'x '(2 . ((a b) (c d)))) '(0 1))))
(assert (struct-eqv? 'p     (sub-binding (pat 'pat '(0 . p))           '(0))))

(define (syntax-error message)
  (writeln message)
  (raise (make-syntax-violation message)))

(define (combine-counts m n)
  (if m
      (if n
          (if (eqv? m n)
              m
              (syntax-error "variables don't match ellipses"))
          m)
      (if n
          n
          (syntax-error "too many ellipses"))))
              
(define (repeat-count tmpl pos mr)
  (notrace 'repeat-count (syntax->datum tmpl) pos)
  (if (identifier? tmpl)
      ((lambda (binding)
         (if (if (pattern-binding? binding)
                 (> (car (binding-value binding)) (length pos)) #f)
             (length (sub-binding binding pos))
             #f))
       (env-lookup mr (syntax-object-expr tmpl)))
      (if (car-ellipsis? tmpl)
          #f
          (if (cadr-ellipsis? tmpl)
              (combine-counts
               (repeat-count (syntax-car tmpl) pos mr)
               (repeat-count (syntax-cddr tmpl) pos mr))
              (if (pair? tmpl)
                  (combine-counts
                   (repeat-count (car tmpl) pos mr)
                   (repeat-count (cdr tmpl) pos mr))
                  #f)))))

(define e (make-environment '()))
(env-add-binding! e (lex 42))
(env-add-binding! e (pat 'pat '(0 . p)))
(env-add-binding! e (pat 'p3  '(1 . (a b c))))
(env-add-binding! e (pat 'p3x '(2 . (() (a) (a b)))))

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
  (notrace '- 'env-lookup (env-lookup mr (syntax-object-expr x)))
  (if (identifier? x)
       (pattern-binding? (env-lookup mr (syntax-object-expr x))) #f))

(define (expand tmpl mr)
  (define (_ tmpl pos ellipsis-ok?)
    (notrace 'expand (syntax-object-expr tmpl) pos ellipsis-ok?)
    (if (identifier? tmpl)
        (if (pattern-variable? tmpl mr)
            ((lambda (binding)
               (if (> (car (binding-value binding)) (length pos))
                   (syntax-error "not enough ellipses"))
               (datum->syntax tmpl (sub-binding binding pos)))
             (env-lookup mr (syntax-object-expr tmpl)))
            tmpl)
        (if (if ellipsis-ok? (car-ellipsis? tmpl) #f) ; (... tmpl)
            (begin
              ((lambda (expr)
                 (if (if (pair? (cdr expr)) (not (null? (cddr expr))) #t)
                         (syntax-error "misplaced ellipsis")))
               (syntax-object-expr tmpl))
              (_ (syntax-cadr tmpl) pos #f))
            (if (if ellipsis-ok? (cadr-ellipsis? tmpl) #f) ;(subtmpl ... . rest)
                ((lambda (loop)
                   (set! loop
                         (lambda (count rest)
                           (if (< count 0)
                               rest
                               (loop (- count 1)
                                     (cons (_ (syntax-car tmpl)
                                              (cons count pos) #t)
                                           rest)))))
                   (loop (- (repeat-count (syntax-car tmpl) pos mr) 1)
                         (_ (syntax-cddr tmpl) pos #t)))
                 '())

                (if (syntax-pair? tmpl)
                    (cons (_ (syntax-car tmpl) pos ellipsis-ok?)
                          (_ (syntax-cdr tmpl) pos ellipsis-ok?))
                    (if (syntax-vector? tmpl)
                        (syntax-list->syntax-vector
                         (_ (syntax-vector->syntax-list tmpl)
                            pos
                            ellipsis-ok?))
                        (if (syntax-null? tmpl)
                            '()
                            tmpl)))))))
  (_ tmpl '() #t))

(define (test-expand input => expected)
  (notrace 'test-expand input '=>? expected)
  (assert (eq? => '=>))
  ((lambda (actual)
     (trace 'expand input '=> actual)
     (if (not (struct-eqv? actual expected))
         (begin
           (trace 'expand input '=> actual)
           (assert (struct-eqv? actual expected)))))
   (syntax->datum (expand (null-wrap input) e))))

(test-expand 'lex                 '=> 'lex)
(test-expand 'pat                 '=> 'p)
(test-expand '(lex . pat)         '=> '(lex . p))
(test-expand '(p3 ...)            '=> '(a b c))
(test-expand '(p3 ... pat)        '=> '(a b c p))
(test-expand '(p3 ... p3 ... pat) '=> '(a b c a b c p))
(test-expand '((p3x ... pat) ...) '=> '((p) (a p) (a b p)))
(test-expand '(... ...)           '=> '...)
(test-expand '(x y (... ...))     '=> '(x y ...))
(test-expand '(... (pat ...))     '=> '(p ...))

(define (exp-syntax x r mr)
  (expand x mr))

(exit)

 ;;; Local Variables: ***
 ;;; indent-tabs-mode: nil ***
 ;;; End: ***
