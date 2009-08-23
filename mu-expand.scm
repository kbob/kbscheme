; This raw lambda is the expander.  It is called whenever
; a lambda form is evaluated.

(lambda (form)

; Named let
; (let              ((lambda (v0 v1 ...)
;   ((v0 i0)           body)
;    (v1 i1)   =>    i0 i1 ...)
;    ...)
;   body)
;
; Unnamed let
; ???

  (define (expand-let form)
    (define bindings (cadr form))
    (define body (cddr form))
    (define (formals bindings)
      (if (null? bindings)
          '()
          (cons (caar bindings) (formals (cdr bindings)))))
    (define (actuals bindings)
      (if (null? bindings)
          '()
          (cons (cadar bindings) (actuals (cdr bindings)))))
    (if (pair? (cadr form))
    (cons (cons 'lambda
                (cons (formals bindings)
                      body))
          (actuals bindings))))

  (define (traverse form)
    (if (pair? form)
	(if (eq? (car form) 'let)
	    (expand-let form)
	    (cons (traverse (car form)) (traverse (cdr form))))
	form))
  (traverse form))
