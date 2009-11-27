; This file contains a sequence of library forms as defined in
; r6rs chapter 7.  The interpreter reads and evaluates them all
; and adds their exports to the global library list.

#;(library (begin-log)
 (export)
 (import (draft))
 (draft-print "loading r6rs"))

(library
 (rnrs base (6))
 (export
   ; From (rnrs base (6))
;;;;define				; r6rs 11.2 Definitions
    define-syntax
;;;;quote				; r6rs 11.4.1 Quotation
;;;;lambda				; r6rs 11.4.2 Procedures
;;;;if					; r6rs 11.4.3 Conditionals
;;;;set!				; r6rs 11.4.4 Assignments
;   cond				; r6rs 11.4.5 Derived conditionals
;   case
;   and
;   or
;   let					; r6rs 11.4.6 Binding constructs
;   let*
;   letrec
;;;;letrec*
;   let-values
;   let*-values
;;;;begin				; r6rs 11.4.7 Sequencing
;;;;eqv?				; r6rs 11.5 Equivalence predicates
;;;;eq?
;   equal?
;;;;procedure?				; r6rs 11.6 Procedure predicate
;;;;number?				; r6rs 11.7.4 Numerical operations
;   complex?
;   real?
;   rational?
;;;;integer?
;   real-valued?
;   rational-valued?
;   integer-valued?
;   exact?
;   inexact?
;   inexact
;   exact
;;;;=
;;;;<
;;;;>
;;;;<=
;;;;>=
    zero?
;   positive?
;   negative?
;   odd?
;   even?
;   finite?
;   infinite?
;   nan?
;   max
;   min
;;;;+
;;;;*
;;;;-
;   /
;;;;abs
;   div-and-mod
;;;;div
;;;;mod
;   div0-and-mod0
;   div0
;   mod0
;   gcd
;   lcm
;   numerator
;   denominator
;   floor
;   ceiling
;   truncate
;   round
;   rationalize
;   exp
;   log
;   sin
;   cos
;   tan
;   asin
;   acos
;   atan
;   sqrt
;   not
;   exact-integer-sqrt
;   expt
;   make-rectangular
;   make-polar
;   real-part
;   imag-part
;   magnitude
;   angle
;   number->string
;   string->number
;;;;not					; r6rs 11.8 Booleans
;;;;boolean?
;;;;pair?				; r6rs 11.9 Pairs and lists
;;;;cons
;;;;car
;;;;cdr
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
;;;;null?
    list?
    list
    length
    append
    reverse
;   list-tail
    list-ref
    map
    for-each
;;;;symbol?				; r6rs 11.10 Symbols
;;;;symbol->string
;;;;symbol=?
;;;;string->symbol
;;;;char?				; r6rs 11.11 Characters
;;;;char->integer
;;;;integer->char
;;;;char=?
;;;;char<?
;;;;char>?
;;;;char<=?
;;;;char>=?
;;;;string?				; r6rs 11.12 Strings
;;;;make-string
;;;;string
;;;;string-length
;;;;string-ref
;;;;string=?
;;;;string<?
;;;;string>?
;;;;string<=?
;;;;string>=?
;;;;substring
;;;;string-append
;;;;string->list
;;;;list->string
    string-for-each
;   string-copy
;;;;vector?				; r6rs 11.13 Vectors
;;;;make-vector
    vector
;;;;vector-length
;;;;vector-ref
;;;;vector-set!
    vector->list
    list->vector
;   vector-fill!
    vector-map
;   vector-for-each
;   error				; r6rs 11.14 Errors and violations
;   assert
;;;;apply				; r6rs 11.15 Control features
;;;;call-with-current-continuation
;;;;call/cc
;   values
;   call-with-values
;   dynamic-wind
;   quasiquote				; r6rs 11.17 Quasiquotation
;   unquote
;   unquote-splicing
;   let-syntax				; r6rs 11.18 Binding constructs
;   letrec-syntax			; for syntactic keywords
    syntax-rules			; r6rs 11.19 Macro transformers
;   identifier-syntax
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

;   (and) => #t
;   (and a) => a
;   (and a b) => (if a b #f)
;   (and a b c) => (if a (if b c #f) #f)
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
    (lambda (wrapped-syntax-object)
      (define form (syntax->datum wrapped-syntax-object))
      (define (get-vars bindings)
	(if (null? bindings)
	  '()
	  (cons (caar bindings) (get-vars (cdr bindings)))))
      (define (get-exprs bindings)
	(if (null? bindings)
	  '()
	  (cons (cadar bindings) (get-vars (cdr bindings)))))
      (if (pair? (car form))
	  (cons (cons 'lambda
		      (cons (get-vars (car form))
			    (cddr form))
		(get-exprs (car form)))
	  (cons (cons 'lambda
		      (cons (cons (cadr form) '())
			    (cons (cadr form) (get-exprs (caddr form)))))
		(cons 'lambda (cons (get-vars (caddr form))
				    (cdddr form))
		      '()))))))

#;(define-syntax let
    (syntax-rules ()
      ((_ ((var init) ...)		; unnamed let
	  body ...)
       ((lambda (var ...)
	  body ...)
	init ...))

      ((_ label				; named let
	  ((var init) ...)
	  body ...)
       ((lambda (label)
	  (set! label (lambda (var ...)
			body ...))
	  (label init ...))
	'()))))


;  (define-syntax (or . tests) ())

  (define (zero? n)
    (= n 0))

;   someday I could rewrite these with a macro.
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

; XXX need unit tests for all of these

  (define (list? obj)
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

  (define (list . args)
    args)

  (define (length list)
    (define (_ list count)
      (if (null? list)
          count
          (_ (cdr list) (+ count 1))))
    (_ list 0))

  (define (append . lists)
    (define (_  list lists)
      (if (null? list)
	  (if (null? lists)
	      '()
	      (if (null? (cdr lists))
		  (car lists)
		  (_ (car lists) (cdr lists))))
	  (cons (car list) (_ (cdr list) lists))))
    (_ '() lists))

  (define (reverse list)
    (define (_ list tail)
      (if (null? list)
          tail
          (_ (cdr list) (cons (car list) tail))))
    (_ list '()))

  (define (list-ref list k)
    (if (= k 0)
	(car list)
	(list-ref (cdr list) (- k 1))))

  (define (map proc list . lists)
    (define (cars lists)
      (if (null? lists)
	  '()
	  (cons (car (car lists)) (cars (cdr lists)))))
    (define (cdrs lists)
      (if (null? lists)
	  '()
	  (cons (cdr (car lists)) (cdrs (cdr lists)))))
    (define (_ lists result)
      (if (null? (car lists))
	  (reverse result)
	  (_ (cdrs lists) (cons (apply proc (cars lists)) result))))
    (_ (cons list lists) '()))

  (define (for-each proc list . lists)
    ; XXX should fail if lists have different lengths. (See for-all.)
    (define (cars lists)
      (if (null? lists)
	  '()
	  (cons (caar lists) (cars (cdr lists)))))
    (define (cdrs lists)
      (if (null? lists)
	  '()
	  (cons (cdar lists) (cdrs (cdr lists)))))
    (define (_ lists)
      (if (null? list)
	  (if #f #f)
	  (begin
	    (apply proc (cars lists))
	    (_ (cdrs lists)))))
    (_ (cons list lists)))

  (define (string-for-each proc string . strings)
    (define (chars index strings)
      (if (null? strings)
	  '()
	  (cons
	   (string-ref (car strings) index)
	   (chars index (cdr strings)))))
    (define (_ index)
      (if (>= index (string-length string))
	  (if #f #f)
	  (begin
	    (apply proc (chars index (cons string strings)))
	    (_ (+ index 1)))))
    (_ 0))

  (define (vector . objs)
    (list->vector objs))

  (define (vector->list vec)
    (define (_ index tail)
      (if (< index 0)
	  tail
	  (_ (- index 1) (cons (vector-ref vec index) tail))))
    (_ (- (vector-length vec) 1) '()))

  (define (list->vector l)
    (define vec (make-vector (length l)))
    (define (_ index tail)
      (if (null? tail)
	  vec
	  (begin
	    (vector-set! vec index (car tail))
	    (_ (+ index 1) (cdr tail)))))
    (_ 0 l))

  (define (vector-map proc vector . vectors)
    ; XXX should check that all vectors are the same length.
    (define (vector-refs vectors i)
      (if (null? vectors)
	  '()
	  (cons (vector-ref (car vectors) i)
		(vector-refs (cdr vectors) i))))
    (define (_ i)
      (if (= i (vector-length vector))
	  '()
	  (cons (apply proc (vector-refs (cons vector vectors) i))
		(_ (+ i 1)))))
    (list->vector (_ 0)))

  (define-syntax syntax-rules
    (lambda (template-stx)

      (define (ext-env var value env)
	(cons (cons var value) env))

      (define (mapcar-improper proc ilist)
	(define (_ partial ilist)
	  (draft-print (list '_ partial ilist))
	  (if (null? ilist)
	      (reverse partial)
	      (if (pair? ilist)
		  (_ (cons (proc (car ilist)) partial) (cdr ilist))
		  (append (reverse partial) (proc ilist)))))
	(_ '() ilist))

      (define (match-ellipsis form pattern literals env)
	; pattern is the pattern after the ellipsis.
	(define (_ head tail)
	  (define v (match tail pattern literals env))
	  (if v
	      (ext-env '... (reverse head) v)
	      (if (null? head)
		  #f
		  (_ (cdr head) (cons (car head) tail)))))
	(_ (reverse form) ()))

      (define (expand-rules form-stx)
	(define form (syntax->datum stx))
	(define name (car form))
	(define template (syntax->datum template-stx))
	(define literals (cadr template))
	(define rules (cddr rules))
	(display (list 'expand-rules 'form form))
	(display (list 'expand-rules 'name name))
	(display (list 'expand-rules 'template template))
	(display (list 'expand-rules 'literals literals))
	(display (list 'expand-rules 'rules rules))
	#f
	)

      (lambda (form-stx)
	(datum->syntax form-stx (expand-rules form-stx)))))

)

(library
 (rnrs lists (6))
 (export
;   find				; r6rs-lib 3, Lists
    for-all
;   exists
;   filter
;   partition
;   fold-left
;   fold-right
    remp
;   remove
    remv
    remq
    memp
;   member
    memv
    memq
    assp
;   assoc
    assv
    assq
;   cons*
    )
 (import
  (draft)
  (rnrs base (6)))

  (define (for-all proc list . lists)
    (define (any-nulls? lists)
      (if (null? lists)
	  #f
	  (if (null? (car lists))
	      #t
	      (any-nulls? (cdr lists)))))
    (define (all-nulls? lists)
      (if (null? lists)
	  #t
	  (if (null? (car lists))
	      (all-nulls? (cdr lists))
	      #f)))
    (define (nulls? lists)
      (if (any-nulls? lists)
          (if (all-nulls? lists)
              #t
              (error-uneven-list-lengths))
          #f))
    (define (cars lists)
      (if (null? lists)
	  '()
	  (cons (caar lists) (cars (cdr lists)))))
    (define (cdrs lists)
      (if (null? lists)
	  '()
	  (cons (cdar lists) (cdrs (cdr lists)))))
    (define (_ lists)
      (if (nulls? lists)
	  #t
	  (if (nulls? (cdr lists))
	      (apply proc (cars lists))
	      (if (apply proc (cars lists))
		  (apply for-all proc (cdrs lists))
		  #f))))
    (_ (cons list lists)))

  ; XXX use this when we have macros.
  #;(define-syntax _accum-cmp
    (syntax-rules ()
      ((_ accum cmp obj list)
       (accum (lambda (x) (cmp x obj)) list))))

  (define (_accum-cmp accum cmp obj list)
    (accum (lambda (x) (cmp x obj)) list))

  (define (remp proc list)
    (define (_ list result)
      (if (null? list)
	  (reverse result)
	  (if (proc (car list))
	      (_ (cdr list) result)
	      (_ (cdr list) (cons (car list) result)))))
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
	(if (proc (caar alist))
	    (car alist)
	    (assp proc (cdr alist)))))

  (define (assoc obj alist)
    (_accum-cmp assp equal? obj alist))

  (define (assv obj alist)
    (_accum-cmp assp eqv? obj alist))

  (define (assq obj alist)
    (_accum-cmp assp eq? obj alist))
)

(library
 (rnrs (6))
 (export

    ; From (rnrs base (6))
    define				; r6rs 11.2 Definitions
    define-syntax
    quote				; r6rs 11.4.1 Quotation
    lambda				; r6rs 11.4.2 Procedures
    if					; r6rs 11.4.3 Conditionals
    set!				; r6rs 11.4.4 Assignments
;   cond				; r6rs 11.4.5 Derived conditionals
;   case
;   and
;   or
;   let					; r6rs 11.4.6 Binding constructs
;   let*
;   letrec
    letrec*
;   let-values
;   let*-values
    begin				; r6rs 11.4.7 Sequencing
    eqv?				; r6rs 11.5 Equivalence predicates
    eq?
;   equal?
    procedure?				; r6rs 11.6 Procedure predicate
    number?				; r6rs 11.7.4 Numerical operations
;   complex?
;   real?
;   rational?
    integer?
;   real-valued?
;   rational-valued?
;   integer-valued?
;   exact?
;   inexact?
;   inexact
;   exact
    =
    <
    >
    <=
    >=
    zero?
;   positive?
    negative?
;   odd?
;   even?
;   finite?
;   infinite?
;   nan?
;   max
;   min
    +
    *
    -
;   /
    abs
;   div-and-mod
    div
    mod
;   div0-and-mod0
;   div0
;   mod0
;   gcd
;   lcm
;   numerator
;   denominator
;   floor
;   ceiling
;   truncate
;   round
;   rationalize
;   exp
;   log
;   sin
;   cos
;   tan
;   asin
;   acos
;   atan
;   sqrt
;   exact-integer-sqrt
;   expt
;   make-rectangular
;   make-polar
;   real-part
;   imag-part
;   magnitude
;   angle
;   number->string
;   string->number
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
    append
    reverse
;   list-tail
    list-ref
    map
    for-each
    symbol?				; r6rs 11.10 Symbols
    symbol->string
    symbol=?
    string->symbol
    char?				; r6rs 11.11 Characters
    char->integer
    integer->char
    char=?
    char<?
    char>?
    char<=?
    char>=?
    string?				; r6rs 11.12 Strings
    make-string
    string
    string-length
    string-ref
    string=?
    string<?
    string>?
    string<=?
    string>=?
    substring
    string-append
    string->list
    list->string
    string-for-each
    string-copy
    vector?				; r6rs 11.13 Vectors
    make-vector
    vector
    vector-length
    vector-ref
    vector-set!
    vector->list
    list->vector
;   vector-fill!
    vector-map
;   vector-for-each
;   error				; r6rs 11.14 Errors and violations
;   assert
    apply				; r6rs 11.15 Control features
    call-with-current-continuation
    call/cc
;   values
;   call-with-values
;   dynamic-wind
;   quasiquote				; r6rs 11.17 Quasiquotation
;   unquote
;   unquote-splicing
;   let-syntax				; r6rs 11.18 Binding constructs
;   letrec-syntax			; for syntactic keywords
    syntax-rules			; r6rs 11.19 Macro transformers
;   identifier-syntax

    ; From (rnrs unicode (6))
;   char-upcase				; r6rs-lib 1.1, Unicode/Characters
;   char-downcase
;   char-titlecase
;   char-foldcase
;   char-ci=?
;   char-ci<?
;   char-ci>?
;   char-ci<=?
;   char-ci>=?
;   char-alphabetic?
;   char-numeric?
;   char-whitespace?
;   char-upper-case?
;   char-lower-case?
;   char-title-case?
    char-general-category
;   string-upcase			; r6rs-lib 1.2, Unicode/Strings
;   string-downcase
;   string-titlecase
;   string-foldcase
;   string-ci=?
;   string-ci<?
;   string-ci>?
;   string-ci<=?
;   string-ci>=?
;   string-normalize-nfd
;   string-normalize-nfkd
;   string-normalize-nfc
;   string-normalize-nfkc

    ; From (rnrs bytevectors (6))
;   endianness				; r6rs-lib 2.2, Bytevectors/General Ops
;   native-endianness
    bytevector?
;   make-bytevector
;   bytevector-length
;   bytevector=?
;   bytevector-fill!
;   bytevector-copy!
;   bytevector-copy
;   bytevector-u8-ref			; 2.3, Operations on bytes and octets
;   bytevector-s8-ref
;   bytevector-u8-set!
;   bytevector-s8-set!
;   bytevector->u8-list
;   u8-list->bytevector
;   bytevector-uint-ref			; 2.4, Operations on integers of
;   bytevector-sint-ref			;      arbitrary size
;   bytevector-uint-set!
;   bytevector-sint-set!
;   bytevector->uint-list
;   uint-list->bytevector
;   bytevector-u16-ref			; 2.5, Operations on 16-bit integers
;   bytevector-s16-ref
;   bytevector-u16-native-ref
;   bytevector-s16-native-ref
;   bytevector-u16-set!
;   bytevector-s16-set!
;   bytevector-u16-native-set!
;   bytevector-s16-native-set!
;   bytevector-u32-ref			; 2.6, Operations on 32-bit integers
;   bytevector-s32-ref
;   bytevector-u32-native-ref
;   bytevector-s32-native-ref
;   bytevector-u32-set!
;   bytevector-s32-set!
;   bytevector-u32-native-set!
;   bytevector-s32-native-set!
;   bytevector-u64-ref			; 2.7, Operations on 64-bit integers
;   bytevector-s64-ref
;   bytevector-u64-native-ref
;   bytevector-s64-native-ref
;   bytevector-u64-set!
;   bytevector-s64-set!
;   bytevector-u64-native-set!
;   bytevector-s64-native-set!
;   bytevector-ieee-single-native-ref	; 2.8, Operations on 64-bit integers
;   bytevector-ieee-single-ref
;   bytevector-ieee-double-native-ref
;   bytevector-ieee-double-ref
;   bytevector-ieee-single-native-set!
;   bytevector-ieee-single-set!
;   bytevector-ieee-double-native-set!
;   bytevector-ieee-double-set!
;   string->utf8			; 2.9, Operations on strings
;   string->utf16
;   string->utf32
;   utf8->string
;   utf16->string
;   utf32->string

    ; From (rnrs lists (6))
;   find				; r6rs-lib 3, Lists
    for-all
;   exists
;   filter
;   partition
;   fold-left
;   fold-right
    remp
;   remove
    remv
    remq
    memp
;   member
    memv
    memq
    assp
;   assoc
    assv
    assq
;   cons*

    ; From (rnrs sorting (6))
;   list-sort				; r6rs-lib 4, Sorting
;   vector-sort
;   vector-sort!

    ; From (rnrs control (6))
;   when				; r6rs-lib 5, Control structures
;   unless
;   do
;   case-lambda

    ; From (rnrs records syntactic (6))
;   define-record-type			; r6rs-lib 6.2, Syntactic layer
;   record-type-descriptor
;   record-constructor-descriptor

    ; From (rnrs records procedural (6))
;   make-record-type-descriptor		; r6rs-lib 6.3, Procedural layer
;   record-type-descriptor?
;   make-record-constructor-descriptor
;   record-constructor
;   record-predicate
;   record-accessor
;   record-mutator

    ; From (rnrs records inspection (6))
;   record?
;   record-rtd
;   record-type-name
;   record-type-parent
;   record-type-uid
;   record-type-generative?
;   record-type-sealed?
;   record-type-opaque?
;   record-type-field-names
;   record-field-mutable?

    ; From (rnrs exceptions (6))
;   with-exception-handler		; r6rs-lib 7.1, Exceptions
;   guard
;   raise
;   raise-continuable

    ; From (rnrs conditions (6))
;   &condition				; r6rs-lib 7.2.1, Condition objects
;   condition
;   simple-conditions
;   condition?
;   condition-predicate
;   condition-accessor
;   define-condition-type
;   &message				; r6rs-lib 7.2.1, Condition types
;   make-message-condition
;   message-condition?
;   condition-message
;   &warning
;   make-warning
;   warning?
;   &serious
;   make-serious-condition
;   serious-condition?
;   &error
;   make-error
;   error?
;   &violation
;   make-violation
;   violation?
;   &assertion
;   make-assertion-violation?
;   assertion-violation?
;   &irritants
;   make-irritants-condition
;   irritants-condition?
;   condition-irritants
;   &who
;   make-who-condition
;   who-condition?
;   condition-who
;   &non-continuable
;   make-non-continuable-violation
;   non-continuable-violation?
;   &implementation-restriction
;   make-implementation-restriction-violation
;   &lexical
;   make-lexical-violation?
;   lexical-violation?
;   &syntax
;   make-syntax-violation?
;   syntax-violation?
;   syntax-violation-form
;   syntax-violation-subform
;   &undefined
;   make-undefined-violation
;   undefined-violation?

    ; From (rnrs io ports (6))
;   &i/o				; r6rs-lib 8.1, I/O Condition types
;   make-i/o-error
;   i/o-error?
;   &i/o-read
;   make-i/o-read-error
;   i/o-read-error?
;   &i/o-write
;   make-i/o-write-error
;   i/o-write-error?
;   &i/o-invalid-position
;   make-i/o-invalid-position-error
;   i/o-invalid-position-error?
;   i/o-error-position
;   &i/o-filename
;   make-i/o-filename-error
;   i/o-filename-error?
;   i/o-error-filename
;   &i/o-file-protection
;   make-i/o-file-protection-error
;   i/o-file-protection-error?
;   &i/o-file-is-read-only
;   make-i/o-file-is-read-only-error
;   i/o-file-is-read-only-error?
;   &i/o-file-already-exists
;   make-i/o-file-already-exists-error
;   i/o-file-already-exists-error?
;   &i/o-file-does-not-exist
;   make-i/o-file-does-not-exist-error
;   i/o-file-does-not-exist-error?
;   &i/o-port
;   make-i/o-port-error
;   i/o-port-error?
;   i/o-error-port
;   file-options			; r6rs-lib 8.2.2, Port I/O/File options
;   buffer-mode				; r6rs-lib 8.2.3, Buffer modes
;   buffer-mode?
;   latin-1-codec			; r6rs-lib 8.2.4, Transcoders
;   utf-8-codec
;   utf-16-codec
;   eol-style
;   native-eol-style
;   &i/o-decoding
;   make-i/o-decoding-error
;   i/o-decoding-error?
;   &i/o-encoding
;   make-i/o-encoding-error
;   i/o-encoding-error?
;   i/o-encoding-error-char
;   error-handling-mode
;   make-transcoder
;   native-transcoder
;   transcoder-codec
;   transcoder-eol-style
;   transcoder-error-handling-mode
;   bytevector->string
;   string->bytevector
    eof-object				; r6rs-lib 8.2.5, End-of-file object
    eof-object?
;   port?			       ; r6rs-lib 8.2.6, Input and output ports
;   port-transcoder
;   textual-port?
;   binary-port?
;   transcoded-port
;   port-has-port-position?
;   port-position
;   port-has-set-port-position!?
;   set-port-position!
;   close-port
;   call-with-port
;   input-port?				; r6rs-lib 8.2.7, Input ports
;   port-eof?
;   open-file-input-port
;   open-bytevector-input-port
;   open-string-input-port
;   standard-input-port
;   current-input-port
;   make-custom-binary-input-port
;   make-custom-textual-input-port
;   get-u8				; r6rs-lib 8.2.8, Binary input
;   lookahead-u8
;   get-bytevector-n
;   get-bytevector-n!
;   get-bytevector-some
;   get-bytevector-all
;   get-char				; r6rs-lib 8.2.9, Textual input
;   lookahead-char
;   get-string-n
;   get-string-n!
;   get-string-all
;   get-line
;   get-datum
;   output-port?			; r6rs-lib 8.2.10, Output ports
;   flush-output-port
;   output-port-buffer-mode
;   open-file-output-port
;   open-bytevector-output-port
;   call-with-bytevector-output-port
;   open-string-output-port
;   call-with-string-output-port
;   standard-output-port
;   standard-error-port
;   current-output-port
;   current-error-port
;   make-custom-binary-output-port
;   make-custom-textual-output-port
;   put-u8				; r6rs-lib 8.2.11, Binary output
;   put-bytevector
;   put-char				; r6rs-lib 8.2.12, Textual output
;   put-string
;   put-datum
;   open-file-input/output-port		; r6rs-lib 8.2.13, Input/output ports
;   make-custom-binary-input/output-port
;   make-custom-textual-input/output-port

    ; From (rnrs io simple (6))
;   eof-object				; r6rs-lib 8.3, Simple I/O
;   eof-object?
;   call-with-input-file
;   call-with-output-file
;   input-port?
;   output-port?
;   current-input-port
;   current-output-port
;   current-error-port
;   with-input-from-file
;   with-output-to-file
;   open-input-file
;   open-output-file
;   close-input-port
;   close-output-port
;   read-char
;   peek-char
;   read
;   write-char
;   newline
;   display
;   write

    ; From (rnrs files (6))
;   file-exists?			; r6rs-lib 9, File system
;   delete-file

    ; From (rnrs programs (6))
    command-line			; r6rs-lib 10, Command-line access
    exit				;              and exit values

    ; From (rnrs arithmetic fixnums (6))
    fixnum?
;   fixnum-width
;   least-fixnum
;   greatest-fixnum
;   fx=?
;   fx>?
;   fx<?
;   fx>=?
;   fx<=?
    fxzero?
;   fxpositive?
;   fxnegative?
;   fxodd?
;   fxeven?
;   fxmax
;   fxmin
;   fx+
;   fx*
;   fx-
;   fxdiv-and-mod
;   fxdiv
;   fxmod
;   fxdiv0-and-mod0
;   fxdiv0
;   fxmod0
;   fx+/carry
;   fx-/carry
;   fx*/carry
;   fxnot
;   fxand
;   fxior
;   fxxor
;   fxif
;   fxbit-count
;   fxlength
;   fxfirst-bit-set
;   fxbit-set?
;   fxcopy-bit
;   fxbit-field
;   fxcopy-bit-field
;   fxarithmetic-shift
;   fxarithmetic-shift-left
;   fxarithmetic-shift-right
;   fxrotate-bit-field
;   fxreverse-bit-field

    ; From (rnrs arithmetic flonums (6))
;   flonum?    
;   real->flonum
;   fl=?
;   fl<?
;   fl<=?
;   fl>?
;   fl>=?
;   flinteger?
;   flzero?
;   flpositive?
;   flnegative?
;   flodd?
;   fleven?
;   flfinite?
;   flinfinite?
;   flnan?
;   flmax
;   flmin
;   fl+
;   fl*
;   fl-
;   fl/
;   flabs
;   fldiv-and-mod
;   fldiv
;   flmod
;   fldiv0-and-mod0
;   fldiv0
;   flmod0
;   flnumerator
;   fldenominator
;   flfloor
;   flceiling
;   fltruncate
;   flround
;   flexp
;   fllog
;   fllog
;   flsin
;   flcos
;   fltan
;   flasin
;   flacos
;   flatan
;   flatan
;   flsqrt
;   flexpt
;   &no-infinities
;   make-no-infinities-violation
;   no-infinities-violation?
;   &no-nans
;   make-no-nans-violation
;   no-nans-violation?
;   fixnum->flonum

    ; From (rnrs arithmetic bitwise (6))
;   bitwise-not				; r6rs-lib 11.4, Exact bitwise
;   bitwise-and				;                arithmetic
;   bitwise-ior
;   bitwise-xor
;   bitwise-if
;   bitwise-bit-count
;   bitwise-length
;   bitwise-first-bit-set
;   bitwise-bit-set?
;   bitwise-copy-bit
;   bitwise-bit-field
;   bitwise-copy-bit-field
;   bitwise-arithmetic-shift
;   bitwise-arithmetic-shift-left
;   bitwise-arithmetic-shift-right
;   bitwise-rotate-bit-field
;   bitwise-reverse-bit-field

    ; From (rnrs syntax-case (6))
;   make-variable-transformer		; r6rs-lib 12.3, Tranformers
;   syntax-case				; r6rs-lib 12.4, Parsing input and
;   syntax				;                producing output
;   identifier?				; r6rs-lib 12.5, Identifier predicates
;   bound-identifier=?
;   free-identifier=?
;   syntax->datum			; r6rs-lib 12.6, Syntax-object and
;   datum->syntax			;                datum conversion
;   generate-temporaries		; r6rs-lib 12.7, ... temporaries
;   with-syntax				; r6rs-lib 12.8, Derived forms
;   quasisyntax				;                and procedures
;   syntax-violation			; r6rs-lib 12.9, Syntax violations

    ; From (rnrs hashtables (6))
;   make-eq-hashtable			; r6rs-lib 13.1, Constructors
;   make-eqv-hashtable
;   make-hashtable
;   hashtable?				; r6rs-lib 13.2, Procedures
;   hashtable-size
;   hashtable-ref
;   hashtable-set!
;   hashtable-delete!
;   hashtable-contains?
;   hashtable-update!
;   hashtable-copy
;   hashtable-clear!
;   hashtable-keys
;   hashtable-entries
;   hashtable-equivalence-function	; r6rs-lib 13.3, Inspection
;   hashtable-hash-function
;   hashtable-mutable?
;   equal-hash				; r6rs-lib 13.4, Hash functions
;   string-hash
;   string-ci-hash
;   symbol-hash

    ; From (rnrs enums (6))
;   make-enumeration			; r6rs-lib 14, Enumerations
;   enum-set-universe
;   enum-set-indexer
;   enum-set-constructor
;   enum-set->list
;   enum-set-member?
;   enum-set-subset?
;   enum-set=?
;   enum-set-union
;   enum-set-intersection
;   enum-set-difference
;   enum-set-complement
;   enum-set-projection
;   define-enumeration

    ; XXX AAARRRGH!  These aren't supposed to be defined here, but
    ; we don't have any other way to make them visible.
    ; From (rnrs eval (6))
    eval
;   environment

    ; From (rnrs mutable-pairs (6))
    set-car!
    set-cdr!

    ; From (rnrs mutable-strings (6))
;   string-set!
;   string-fill!

    ; From (rnrs r5rs (6))
;   exact->inexact
;   inexact->exact
;   quotient
;   remainder
;   modulo
;   delay
;   force
;   null-environment
;   scheme-report-environment

    ; From (draft)
    draft-environment
    null-environment
    draft-read
    draft-print
;   mu

    ; From (implementation)
    write
    newline
  )
  (import (rnrs base (6))
	  (rnrs unicode (6))
	  (rnrs bytevector (6))
	  (rnrs lists (6))
	; (rnrs sorting (6))
	; (rnrs control (6))
	; (rnrs records syntactic (6))
	; (rnrs records procedural (6))
	; (rnrs exceptions (6))
	; (rnrs conditions (6))
	  (rnrs io ports (6))
	; (rnrs io simple (6))
	; (rnrs files (6))
	  (rnrs programs (6))
	  (rnrs arithmetic fixnums (6))
	; (rnrs arithmetic flonums (rnrs 6)) (6))
	; (rnrs arithmetic bitwise (rnrs 6)) (6))
	  (rnrs syntax-case (6))
	; (rnrs hashtables (6))
	; (rnrs enums (6))
	  (rnrs eval (6))
	  (rnrs mutable-pairs (6))
	  (draft)
	  (implementation)
  )
)

#;(library (end-log)
 (export)
 (import (draft))
 (draft-print "r6rs loaded"))
