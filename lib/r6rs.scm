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
    length
    reverse
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

;  #;(define-syntax let
;    (lambda wrapped-syntax-object
;      (define (get-vars bindings)
;	(if (null? bindings)
;	  '()
;	  (cons (caar bindings) (get-vars (cdr bindings)))))
;      (define (get-exprs bindings)
;	(if (null? bindings)
;	  '()
;	  (cons (cadar bindings) (get-vars (cdr bindings)))))
;      (draft-print form)
;      (if (pair? (car form))
;	  (cons (cons 'lambda
;		      (cons (get-vars (car form))
;			    body))
;		(get-exprs (car form)))
;	  (raise &syntax))))		; named let unimplemented
    
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

  (define (length list)
    (define (ll list count)
      (if (null? list)
          count
          (ll (cdr list) (+ count 1))))
    (ll list 0))

  (define (reverse list)
    (define (rr list tail)
      (if (null? list)
          tail
          (rr (cdr list) (cons (car list) tail))))
    (rr list '()))

)

(library
 (rnrs (6))
 (export

    ; (rnrs base (6))
    define				; r6rs 11.2 Definitions
  ; define-syntax
    quote				; r6rs 11.4.1 Quotation
    lambda				; r6rs 11.4.2 Procedures
    if					; r6rs 11.4.3 Conditionals
    set!				; r6rs 11.4.4 Assignments
  ; cond				; r6rs 11.4.5 Derived conditionals
  ; case
  ; and
  ; or
  ; let					; r6rs 11.4.6 Binding constructs
  ; let*
  ; letrec
  ; letrec*
  ; let-values
  ; let*-values
    begin				; r6rs 11.4.7 Sequencing
  ; eqv?				; r6rs 11.5 Equivalence predicates
    eq?
  ; equal?
    procedure?				; r6rs 11.6 Procedure predicate
    number?				; r6rs 11.7.4 Numerical operations
  ; complex?
  ; real?
  ; rational?
    integer?
  ; real-valued?
  ; rational-valued?
  ; integer-valued?
  ; exact?
  ; inexact?
  ; inexact
  ; exact
    =
    <
    >
    <=
    >=
  ; zero?
  ; positive?
  ; negative?
  ; odd?
  ; even?
  ; finite?
  ; infinite?
  ; nan?
  ; max
  ; min
    +
    *
    -
  ; /
    abs
  ; div-and-mod
    div
    mod
  ; div0-and-mod0
  ; div0
  ; mod0
  ; gcd
  ; lcm
  ; numerator
  ; denominator
  ; floor
  ; ceiling
  ; truncate
  ; round
  ; rationalize
  ; exp
  ; log
  ; sin
  ; cos
  ; tan
  ; asin
  ; acos
  ; atan
  ; sqrt
  ; not
  ; exact-integer-sqrt
  ; expt
  ; make-rectangular
  ; make-polar
  ; real-part
  ; imag-part
  ; magnitude
  ; angle
  ; number->string
  ; string->number
    not					; r6rs 11.8 Booleans
    boolean?
    pair?				; r6rs 11.9 Pairs and lists
    cons
    car
    cdr
    caar
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
    null?
    list?
    list
    length
  ; append
    reverse
  ; list-tail
  ; list-ref
  ; map
  ; for-each
    symbol?				; r6rs 11.10 Symbols
  ; symbol->string
  ; symbol=?
  ; string->symbol
    char?				; r6rs 11.11 Characters
    char->integer
    integer->char
    char=?
    char<?
    char>?
    char<=?
    char>=?
    string?				; r6rs 11.12 Strings
  ; make-string
  ; string
  ; string-length
  ; string-ref
  ; string=?
  ; string<?
  ; string>?
  ; string<=?
  ; string>=?
  ; substring
  ; string-append
  ; string->list
  ; list->string
  ; string-for-each
  ; string-copy
    vector?				; r6rs 11.13 Vectors
    make-vector
    vector-length
    vector-ref
    vector-set!
  ; vector->list
  ; list->vector
  ; vector-fill!
  ; vector-map
  ; vector-for-each
  ; error				; r6rs 11.14 Errors and violations
  ; assert
    apply				; r6rs 11.15 Control features
    call-with-current-continuation
    call/cc
  ; values
  ; call-with-values
  ; dynamic-wind
  ; quasiquote				; r6rs 11.17 Quasiquotation
  ; unquote
  ; unquote-splicing
  ; let-syntax				; r6rs 11.18 Binding constructs
  ; letrec-syntax			; for syntactic keywords
  ; syntax-rules    			; r6rs 11.19 Macro transformers
  ; identifier-syntax

    ; (rnrs arithmetic fixnums (6))
    fixnum?

    ; (rnrs unicode (6))
    char-general-category

    ; XXX AAARRRGH!  These aren't supposed to be defined here, but
    ; we don't have any other way to make them visible.
    ; (rnrs eval (6))
    eval

    ; (rnrs mutable-pairs (6))
    set-car!
    set-cdr!

    ; (draft)
    draft-environment
    draft-read
    draft-print
  )
  (import (rnrs base (6))
	  (rnrs arithmetic fixnums (6))
	  (rnrs unicode (6))
	  (rnrs eval (6))
	  (rnrs mutable-pairs (6))
	  (draft)
  )
)
