; This file contains a sequence of library forms as defined in
; r6rs chapter 7.  The interpreter reads and evaluates them all
; and adds their exports to the global library list.

(library
 (rnrs base (6))
 (export
;  quote				; r6rs 11.4.1 Quotation

;    cond				; r6rs 11.4.5 Derived conditionals
;    case
;  and
;    or

;   let					;r6rs 11.4.6 Binding constructs

    caar				; r6rs 11.9 Pairs and lists
    cadr
    cdar
    cddr
    caaar
    caadr
    cadar
    caddr
    cdaar
    cdadr
    cddar
    cdddr
    caaaar
    caaadr
    caadar
    caaddr
    cadaar
    cadadr
    caddar
    cadddr
    cdaaar
    cdaadr
    cdadar
    cdaddr
    cddaar
    cddadr
    cdddar
    cddddr
    list?
    list
    )

 (import (draft) (rnrs base (6)))

; 11.4.1.  Quotation
;
; (quote <datum>)			# syntax

; (define-syntax quote
;   (lambda (datum) datum))

; 11.4.5.  Derived conditionals
;
; (cond <cond clause1> <cond clause2> ...)         # syntax ;
; => auxiliary syntax
; => auxiliary syntax
;
; (case <key> <case clause 1> <case clause 2> ...) # syntax
;
; (and <test1> ...)				    # syntax
;
; (or <test1> ...)				    # syntax

;  (define-syntax (cond . clauses) () ...)
;  (define-syntax (case key . clauses) () ...)

  ; (and) => #t
  ; (and a) => a
  ; (and a b) => (if a b #f)
  ; (and a b c) => (if a (if b c #f) #f)
;  (define-syntax and
;    (lambda tests
;;      (draft-print tests)
;      (if (null? tests)
;	  #t
;	  (if (null? (cdr tests))
;		     (car tests)
;		     (list 'if (car tests) (and (cdr tests)) #f)))))


; 11.4.6.  Binding constructs
;
; (let <bindings> <body>)		# syntax
;
; (let* <bindings> <body>)		# syntax
;
; (letrec <bindings> <body>)		# syntax
;
; (letrec* <bindings> <body>)		# syntax
;
; (let-values <mv-bindings> <body>)	# syntax
;
; (let*-values <mv-bindings> <body>)	# syntax

  #;(define-syntax let
    (lambda wrapped-syntax-object
      (define (get-vars bindings)
	(if (null? bindings)
	  '()
	  (cons (caar bindings) (get-vars (cdr bindings)))))
      (define (get-exprs bindings)
	(if (null? bindings)
	  '()
	  (cons (cadar bindings) (get-vars (cdr bindings)))))
      (draft-print form)
      (if (pair? (car form))
	  (cons (cons 'lambda
		      (cons (get-vars (car form))
			    body))
		(get-exprs (car form)))
	  (raise &syntax))))		; named let unimplemented
    
;  (define-syntax (or . tests) ())

  ; someday I could rewrite these with a macro.
  (define (caar pair) (car (car pair)))
  (define (cadr pair) (car (cdr pair)))
  (define (cdar pair) (cdr (car pair)))
  (define (cddr pair) (cdr (cdr pair)))
  (define (caaar pair) (car (car (car pair))))
  (define (caadr pair) (car (car (cdr pair))))
  (define (cadar pair) (car (cdr (car pair))))
  (define (caddr pair) (car (cdr (cdr pair))))
  (define (cdaar pair) (cdr (car (car pair))))
  (define (cdadr pair) (cdr (car (cdr pair))))
  (define (cddar pair) (cdr (cdr (car pair))))
  (define (cdddr pair) (cdr (cdr (cdr pair))))
  (define (caaaar pair) (car (car (car (car pair)))))
  (define (caaadr pair) (car (car (car (cdr pair)))))
  (define (caadar pair) (car (car (cdr (car pair)))))
  (define (caaddr pair) (car (car (cdr (cdr pair)))))
  (define (cadaar pair) (car (cdr (car (car pair)))))
  (define (cadadr pair) (car (cdr (car (cdr pair)))))
  (define (caddar pair) (car (cdr (cdr (car pair)))))
  (define (cadddr pair) (car (cdr (cdr (cdr pair)))))
  (define (cdaaar pair) (cdr (car (car (car pair)))))
  (define (cdaadr pair) (cdr (car (car (cdr pair)))))
  (define (cdadar pair) (cdr (car (cdr (car pair)))))
  (define (cdaddr pair) (cdr (car (cdr (cdr pair)))))
  (define (cddaar pair) (cdr (cdr (car (car pair)))))
  (define (cddadr pair) (cdr (cdr (car (cdr pair)))))
  (define (cdddar pair) (cdr (cdr (cdr (car pair)))))
  (define (cddddr pair) (cdr (cdr (cdr (cdr pair)))))

  (define (list? obj)
    (if (null? obj)			; XXX detect cycles.
	#t
	(if (not (pair? obj))
	    #f
	    (list? (cdr obj)))))

  (define (list . args)
    args)

)
