#include <assert.h>

#include "proc.h"
#include "test.h"

LIBRARY(L"(rnrs base (6))")

/* 11.1.1.  Base types
 *
 *    boolean?		pair?
 *    symbol?		number?
 *    char?		string?
 *    vector?		procedure?
 *    null?
 */

DEFINE_PROC(L"symbol?")
{
    RETURN(make_boolean(is_symbol(pair_car(F_SUBJ))));
}

DEFINE_PROC(L"char?")
{
    RETURN(make_boolean(is_character(pair_car(F_SUBJ))));
}

DEFINE_PROC(L"number?")
{
    /* XXX RETURN(make_boolean(is_number(pair_car(F_SUBJ)))); */
    RETURN(make_boolean(is_fixnum(pair_car(F_SUBJ))));
}

DEFINE_PROC(L"string?")
{
    RETURN(make_boolean(is_string(pair_car(F_SUBJ))));
}

/* 11.2.1.  Variable definitions
 *
 * (define <variable> <expression>)	  # syntax
 * (define <variable>)			  # syntax
 * (define (<variable> <formals>) body)	  # syntax
 * (define (<variable> . <formals>) body) # syntax
 */

DEFINE_BLOCK(b_define_continue)
{
    obj_t *var = pair_car(F_SUBJ);
    env_bind(F_ENV, var, BINDING_MUTABLE, VALUE);
    RETURN(UNSPECIFIED);
}

DEFINE_SPECIAL_FORM(L"define")
{
    AUTO_ROOT(var, pair_car(F_SUBJ));
    AUTO_ROOT(rest, pair_cdr(F_SUBJ));
    obj_t *value;
    if (is_pair(var)) {
	obj_t *formals = pair_cdr(var);
	var = pair_car(var);
	value = make_procedure(rest, formals, F_ENV);
    } else if (is_null(rest)) {
	value = UNSPECIFIED;
    } else {
	assert(is_null(pair_cdr(rest)));
	AUTO_ROOT(exp, pair_car(rest));
	EVAL_THEN_GOTO(exp, F_ENV, b_define_continue, F_SUBJ, F_ENV);
    }
    env_bind(F_ENV, var, BINDING_MUTABLE, value);
    RETURN(UNSPECIFIED);
}

TEST_EVAL(L"(define v0 3) v0",			L"3");
TEST_EVAL(L"(define v1) v1",			UNSPECIFIED_REPR);
TEST_EVAL(L"(define (v2 x) (- x)) v2",		L"(lambda (x) (- x))");
TEST_EVAL(L"(define (v3 x) (- x)) (v3 3)",	L"-3");
TEST_EVAL(L"(define (v4 . x) x) v4",		L"(lambda x x)");
TEST_EVAL(L"(define v5 1)",			UNSPECIFIED_REPR);
TEST_EVAL(L"(define v6)",			UNSPECIFIED_REPR);
/* XXX Test that var can't be redefined. (need exceptions first.) */

/* from r6rs */
TEST_EVAL(L"(define add3\n"
          L"(lambda (x) (+ x 3)))\n"
          L"(add3 3)",                     L"6");
TEST_EVAL(L"(define first car)\n"
          L"(first '(1 2))",               L"1");

/* 11.2.2.  Syntax definitions
 *
 * (define-syntax <keyword> <expression>) # syntax
 */

DEFINE_BLOCK(b_define_syntax_continue)
{
    obj_t *proc = VALUE;
    proc = make_special_form_procedure(procedure_body(proc),
				       procedure_args(proc),
				       procedure_env(proc));
    obj_t *keyword = pair_car(F_SUBJ);
    env_bind(F_ENV, keyword, BINDING_IMMUTABLE, proc);
    RETURN(UNSPECIFIED);
}

DEFINE_SPECIAL_FORM(L"define-syntax")
{
    //AUTO_ROOT(keyword, pair_car(F_SUBJ));
    AUTO_ROOT(exp, pair_cadr(F_SUBJ));
    EVAL_THEN_GOTO(exp, F_ENV, b_define_syntax_continue, F_SUBJ, F_ENV);
}

TEST_EVAL(L"(define-syntax qot (lambda (x) x))\n"
	  L"(qot (1 2))",		L"(1 2)");

/* from r6rs */
//TEST_EVAL(L"(let ()\n"
//	  L"  (define even?\n"
//	  L"    (lambda (x)\n"
//	  L"	  (or (= x 0) (odd? (- x 1)))))\n"
//	  L"  (define-syntax odd?\n"
//	  L"    (syntax-rules ()\n"
//	  L"      ((odd? x) (not (even? x)))))\n"
//	  L"  (even? 10))",		L"#t");
//TEST_EVAL(L"(let ()\n"
//          L"  (define-syntax bind-to-zero\n"
//          L"    (syntax-rules ()\n"
//          L"      ((bind-to-zero id) (define id 0))))\n"
//          L"  (bind-to-zero x)\n"
//          L"  x)",			L"0");


/* 11.4.1.  Quotation
 *
 * (quote <datum>)			# syntax
 */

/* XXX move these to wherever the Scheme unit tests will be. */
TEST_EVAL(L"(quote ())",		L"()");
TEST_EVAL(L"(quote (a b c))",		L"(a b c)");

/* from r6rs */
TEST_EVAL(L"(quote a)",			L"a");
TEST_EVAL(L"(quote #(a b c))",		L"#(a b c)");
TEST_EVAL(L"(quote (+ 1 2))",		L"(+ 1 2)");

//TEST_EVAL(L"'\"abc\"",		L"\"abc\"");
TEST_EVAL(L"'145932",			L"145932");
TEST_EVAL(L"'a",			L"a");
TEST_EVAL(L"'#(a b c)",			L"#(a b c)");
TEST_EVAL(L"'()",			L"()");
TEST_EVAL(L"'(+ 1 2)",			L"(+ 1 2)");
TEST_EVAL(L"'(quote a)",		L"(quote a)");
TEST_EVAL(L"''a",			L"(quote a)");


/* 11.4.2.  Procedures
 *
 * (lambda <formals> <body>)		# syntax
 */

DEFINE_SPECIAL_FORM(L"lambda")
{
    obj_t *params = pair_car(F_SUBJ);
    obj_t *body = pair_cdr(F_SUBJ);
    RETURN(make_procedure(body, params, F_ENV));
}

TEST_EVAL(L"((lambda (x) (+ x 3)) 4)",		L"7");
TEST_EVAL(L"((lambda (x) (+ x x)) 4)",		L"8");
TEST_EVAL(L"((lambda (x) (+) (+ x 3)) 4)",	L"7");
TEST_EVAL(L"((lambda (x) (+ x 3) (+)) 4)",	L"0");

/* from r6rs */
TEST_EVAL(L"(lambda (x) (+ x x))",		L"(lambda (x) (+ x x))");
TEST_EVAL(L"((lambda (x) (+ x x)) 4)",		L"8");
TEST_EVAL(L"((lambda (x)\n"
	  L"   (define (p y)\n"
	  L"     (+ y 1))\n"
	  L"   (+ (p x) x))\n"
	  L" 5)",				L"11");
TEST_EVAL(L"(define reverse-subtract\n"
	  L"  (lambda (x y) (- y x)))\n"
	  L"(reverse-subtract 7 10)",		L"3");
//TEST_EVAL(L"(define add4\n"
//	  L"  (let ((x 4))\n"
//	  L"    (lambda (y) (+ x y))))\n"
//	  L"(add4 6)",				L"10");
//

TEST_EVAL(L"((lambda x x) 3 4 5 6)",		L"(3 4 5 6)");
TEST_EVAL(L"((lambda (x y . z) z)\n"
          L" 3 4 5 6)",				L"(5 6)");

/* 11.4.3.  Conditionals
 *
 * (if <test> <consequent> <alternate>) # syntax
 * (if <test> <consequent>)             # syntax
 */

DEFINE_BLOCK(b_continue_if)
{
    obj_t *truth = VALUE;
    if (!is_boolean(truth) || boolean_value(truth)) {
	obj_t *consequent = pair_car(F_SUBJ);
	TAIL_EVAL(consequent);
    }
    obj_t *cddr = pair_cdr(F_SUBJ);
    if (is_null(cddr))
	RETURN(UNSPECIFIED);
    obj_t *alternate = pair_car(cddr);
    TAIL_EVAL(alternate);
}

DEFINE_SPECIAL_FORM(L"if")
{
    AUTO_ROOT(test, pair_car(F_SUBJ));
    AUTO_ROOT(subj_cdr, pair_cdr(F_SUBJ));
    EVAL_THEN_GOTO(test, F_ENV, b_continue_if, subj_cdr, F_ENV);
}

TEST_EVAL(L"(if (= 0 0) 1 2)",		L"1");
TEST_EVAL(L"(if (= 0 1) 1 2)",		L"2");
TEST_EVAL(L"(if (= 0 0) 1)",		L"1");
TEST_EVAL(L"(if (= 0 1) 1)",		UNSPECIFIED_REPR);

/* from r6rs */
TEST_EVAL(L"(if (> 3 2) 'yes 'no)",	L"yes");
TEST_EVAL(L"(if (> 2 3) 'yes 'no)",	L"no");
TEST_EVAL(L"(if (> 3 2)\n"
          L"    (- 3 2)\n"
          L"    (+ 3 2))",		L"1");
TEST_EVAL(L"(if #f #f)",		UNSPECIFIED_REPR);

/* 11.4.4.  Assignments
 *
 * (set! <variable> <expression>)	# syntax
 */

DEFINE_BLOCK(b_set_continue)
{
    obj_t *var = F_SUBJ;
    obj_t *binding = env_lookup(F_ENV, var);
    assert(binding_is_mutable(binding));
    binding_set(binding, VALUE);
    RETURN(UNSPECIFIED);
}

DEFINE_SPECIAL_FORM(L"set!")
{
    AUTO_ROOT(var, pair_car(F_SUBJ));
    AUTO_ROOT(exp, pair_cadr(F_SUBJ));
    EVAL_THEN_GOTO(exp, F_ENV, b_set_continue, var, F_ENV);
}

TEST_EVAL(L"(define v1 ()) (set! v1 4) v1", L"4");
TEST_EVAL(L"(define v2 ()) (set! v2 4)",    UNSPECIFIED_REPR);

/* from r6rs */
//TEST_EVAL(L"(let ((x 2))\n"
//          L"  (+ x 1)\n"
//          L"  (set! x 4)\n"
//          L"  (+ x 1))",        L"5");
          
/* 11.4.5.  Derived conditionals
 *
 * (cond <cond clause1> <cond clause2> ...)         # syntax
 * => auxiliary syntax
 * => auxiliary syntax
 *
 * (case <key> <case clause 1> <case clause 2> ...) # syntax
 *
 * (and <test1> ...)				    # syntax
 *
 * (or <test1> ...)				    # syntax
 */

/* 11.4.6.  Binding constructs
 *
 * (let <bindings> <body>)		# syntax
 *
 * (let* <bindings> <body>)		# syntax
 *
 * (letrec <bindings> <body>)		# syntax
 *
 * (letrec* <bindings> <body>)		# syntax
 *
 * (let-values <mv-bindings> <body>)	# syntax
 *
 * (let*-values <mv-bindings> <body>)	# syntax
 */

/* 11.4.7.  Sequencing
 *
 * (begin <form> ...)			 # syntax
 * (begin <expression> <expression> ...) # syntax
 */

/* 11.5.  Equivalence
 *
 * (eqv? obj1 obj2)			# procedure
 *
 * (eq? obj1 obj2)			# procedure
 *
 * (equal? obj1 obj2)			# procedure
 */

DEFINE_PROC(L"eq?")
{
    obj_t *obj1 = pair_car(F_SUBJ);
    obj_t *obj2 = pair_cadr(F_SUBJ);
    RETURN(make_boolean(obj1 == obj2));
}

/* from r6rs */
TEST_EVAL(L"(eq? 'a 'a)",               L"#t");
TEST_EVAL(L"(eq? (list 'a) (list 'a))", L"#f");
TEST_EVAL(L"(eq? '() '())",             L"#t");
TEST_EVAL(L"(eq? car car)",             L"#t");
//TEST_EVAL(L"(let ((x '(a)))\n"
//          L"  (eq? x x))",              L"#t");

/* 11.6.  Procedure predicate
 *
 * (procedure? obj)
 */

DEFINE_PROC(L"procedure?")
{
    RETURN(make_boolean(is_procedure(pair_car(F_SUBJ))));
}

/* from r6rs */
TEST_EVAL(L"(procedure? car)",                   L"#t");
TEST_EVAL(L"(procedure? 'car)",                  L"#f");
TEST_EVAL(L"(procedure? (lambda (x) (* x x)))",  L"#t");
TEST_EVAL(L"(procedure? '(lambda (x) (* x x)))", L"#f");

/* 11.7.  Arithmetic
 *
 * ...
 */

/* 11.8.  Booleans
 *
 * (not obj)				# procedure
 *
 * (boolean? obj)			# procedure
 *
 * (boolean=? bool1 bool2 bool3 ...)	# procedure
 */

DEFINE_PROC(L"not")
{
    obj_t *obj = pair_car(F_SUBJ);
    RETURN(make_boolean(obj == make_boolean(false)));
}

TEST_EVAL(L"(not (= 0 0))",     L"#f");
TEST_EVAL(L"(not (= 1 0))",     L"#t");
TEST_EVAL(L"(not +)",           L"#f");

/* from r6rs */
TEST_EVAL(L"(not #t)",          L"#f");
TEST_EVAL(L"(not 3)",           L"#f");
TEST_EVAL(L"(not (list 3))",    L"#f");
TEST_EVAL(L"(not #f)",          L"#t");
TEST_EVAL(L"(not '())",         L"#f");
TEST_EVAL(L"(not (list))",      L"#f");
TEST_EVAL(L"(not 'nil)",        L"#f");

DEFINE_PROC(L"boolean?")
{
    RETURN(make_boolean(is_boolean(pair_car(F_SUBJ))));
}

/* from r6rs */
TEST_EVAL(L"(boolean? #f)",     L"#t");
TEST_EVAL(L"(boolean? 0)",      L"#f");
TEST_EVAL(L"(boolean? '())",    L"#f");

/* 11.9.  Pairs and lists
 *
 * (pair? obj)				# procedure
 *
 * (cons obj1 obj2)			# procedure
 *
 *
 * (car pair)				# procedure
 *
 * (cdr pair)				# procedure
 *
 * (caar pair)				# procedure
 * (cadr pair)				# procedure
 *     ...				     ...
 * (cdddar pair)			# procedure
 * (cddddr pair)			# procedure
 *
 * (null? obj)				# procedure
 *
 * (list? obj)				# procedure
 *
 * (list obj ...)			# procedure
 *
 * (length list)			# procedure
 *
 * (append list ... obj)		# procedure
 *
 * (reverse list)			# procedure
 *
 * (list-tail list k)			# procedure
 *
 * (list-ref list k)			# procedure
 *
 * (map proc list1 list2 ...)		# procedure
 *
 * (for-each proc list1 list2 ...)	# procedure
 */

DEFINE_PROC(L"pair?")
{
    RETURN(make_boolean(is_pair(pair_car(F_SUBJ))));
}

TEST_EVAL(L"(pair? (quote (1 2)))", L"#t");
TEST_EVAL(L"(pair? 12)",            L"#f");
TEST_EVAL(L"(pair? ())",            L"#f");

/* from r6rs */
TEST_EVAL(L"(pair? '(a . b))",      L"#t");
TEST_EVAL(L"(pair? '(a b c))",      L"#t");
TEST_EVAL(L"(pair? '())",           L"#f");
//TEST_EVAL(L"(pair? '#(a b))",       L"#f");

DEFINE_PROC(L"cons")
{
    RETURN(make_pair(pair_car(F_SUBJ),
		     pair_cadr(F_SUBJ)));
}

TEST_EVAL(L"(cons 1 2)",           L"(1 . 2)");
TEST_EVAL(L"(cons '(1 2) '(3 4))", L"((1 2) 3 4)");
TEST_EVAL(L"(cons 1 ())",          L"(1)");

/* from r6rs */
TEST_EVAL(L"(cons 'a '())",        L"(a)");
TEST_EVAL(L"(cons '(a) '(b c d))", L"((a) b c d)");
//TEST_EVAL(L"(cons \"a\" '(b c))",  L"(\"a\" b c)");
TEST_EVAL(L"(cons 'a 3)",          L"(a . 3)");
TEST_EVAL(L"(cons '(a b) 'c)",     L"((a b) . c)");

DEFINE_PROC(L"car")
{
    RETURN(pair_caar(F_SUBJ));
}

TEST_EVAL(L"(car (quote (1 2)))",  L"1");

/* from r6rs */
TEST_EVAL(L"(car '(a b c))",       L"a");
TEST_EVAL(L"(car '((a) b c d))",   L"(a)");
TEST_EVAL(L"(car '(1 . 2))",       L"1");
//TEST_EVAL(L"(car '())",            L"&exception");

DEFINE_PROC(L"cdr")
{
    RETURN(pair_cdar(F_SUBJ));
}

TEST_EVAL(L"(cdr (quote (1 2)))", L"(2)");

/* from r6rs */
TEST_EVAL(L"(cdr '((a) b c d))",   L"(b c d)");
TEST_EVAL(L"(cdr '(1 . 2))",       L"2");
//TEST_EVAL(L"(cdr '())",            L"&exception");

DEFINE_PROC(L"null?")
{
    RETURN(make_boolean(is_null(pair_car(F_SUBJ))));
}

TEST_EVAL(L"(null? ())", L"#t");
TEST_EVAL(L"(null? +)", L"#f");
TEST_EVAL(L"(null? (quote (1 2)))", L"#f");

/* 11.10.  Symbols
 *
 * ...
 */

/* 11.11.  Characters
 *
 * ...
 */

/* 11.12.  Strings
 *
 * ...
 */

/* 11.13.  Vectors
 *
 * (make-vector k)			# procedure
 * (make-vector k fill)			# procedure
 *
 * (vector? obj)			# procedure
 *
 * (vector obj ...)			# procedure
 *
 * (vector-length vector)		# procedure
 *
 * (vector-ref vector k)		# procedure
 *
 * (vector-set! vector k obj)		# procedure
 *
 * (vector->list vector)		# procedure
 * (list->vector list)			# procedure
 * (vector-fill! vector fill)		# procedure
 * (vector-map proc vector1 vector2 ...) # procedure
 *
 * (vector-for-each proc vector1 vector2 ...) # procedure
 */

DEFINE_PROC(L"make-vector")
{
    size_t k = fixnum_value(pair_car(F_SUBJ));
    obj_t *fill = NIL;
    if (pair_cdr(F_SUBJ))
	fill = pair_cadr(F_SUBJ);
    RETURN(make_vector(k, fill));
}

TEST_EVAL(L"(make-vector 3)",	L"#(() () ())");
TEST_EVAL(L"(make-vector 3 4)",	L"#(4 4 4)");

DEFINE_PROC(L"vector?")
{
    RETURN(make_boolean(is_vector(pair_car(F_SUBJ))));
}

TEST_EVAL(L"(vector? '(3 4))",	L"#f");
TEST_EVAL(L"(vector? '())",	L"#f");
TEST_EVAL(L"(vector? '#(3))",	L"#t");

DEFINE_PROC(L"vector-length")
{
    RETURN(make_fixnum(vector_len(pair_car(F_SUBJ))));
}

TEST_EVAL(L"(vector-length '#(a b c))", L"3");

DEFINE_PROC(L"vector-ref")
{
    obj_t *vec = pair_car(F_SUBJ);
    size_t index = fixnum_value(pair_cadr(F_SUBJ));
    RETURN(vector_ref(vec, index));
}

TEST_EVAL(L"(vector-ref '#(0 1 2) 1)",	L"1");

DEFINE_PROC(L"vector-set!")
{
    obj_t *vec = pair_car(F_SUBJ);
    size_t index = fixnum_value(pair_cadr(F_SUBJ));
    obj_t *elem = pair_car(pair_cdr(pair_cdr(F_SUBJ)));
    vector_set(vec, index, elem);
    RETURN(UNSPECIFIED);
}

TEST_EVAL(L"(define a '#(1 2 3))\n"
          L"(vector-set! a 1 'x)\n"
	  L"a",				L"#(1 x 3)");

/* from r6rs */
//TEST_EVAL(L"'#(0 (2 2 2 2) \"Anna\")",	L"#(0 (2 2 2 2) \"Anna\")");
//TEST_EVAL(L"(vector 'a 'b 'c)",		L"#(a b c)");
//TEST_EVAL(L"(list ((vec (vector 0 '(2 2 2 2) \"Anna\")))\n"
//	  L"  (vector-set! vec 1 '(\"Sue\" \"Sue\"))\n"
//	  L"  vec)",			L"#(0 (\"Sue\" \"Sue\") \"Anna\")");
//TEST_EVAL(L"(vector-set! '#(0 1 2) 1 "doe")\n", L"&assertion");

/* 11.14.  Errors and violations
 *
 * ...
 */

/* 11.15. Control features
 *
 * (apply proc arg1 ... rest-args)	# procedure
 *
 * (call-with-current-continuation proc) # procedure
 * (call/cc proc)			# procedure
 *
 * (values obj ...)			# procedure
 *
 * (call-with-values producer consumer	# procedure
 *
 * (dynamic-wind before thunk after)	# procedure
 */

DEFINE_PROC(L"apply")
{
    AUTO_ROOT(proc, pair_car(F_SUBJ));
    AUTO_ROOT(args, pair_cdr(F_SUBJ));
    AUTO_ROOT(arglist, NIL);
    AUTO_ROOT(last_arg, NIL);
    while (args) {
	obj_t *arg;
	if (pair_cdr(args))
	    arg = make_pair(pair_car(args), NIL);
	else
	    arg = pair_car(args);
	if (arglist)
	    pair_set_cdr(last_arg, arg);
	else
	    arglist = arg;
	last_arg = arg;
	args = pair_cdr(args);
    }
    POP_FUNCTION_ROOTS();
    return eval_application(proc, arglist);
}

TEST_EVAL(L"(apply + ())",		L"0");
TEST_EVAL(L"(apply + '(2))",		L"2");
TEST_EVAL(L"(apply + '(2 3))",		L"5");
TEST_EVAL(L"(apply + 2 ())",		L"2");
TEST_EVAL(L"(apply + 2 '(3 4))",	L"9");
TEST_EVAL(L"(apply + 2 '(3 4))",	L"9");
TEST_EVAL(L"(apply + 2 3 '(4 5))",	L"14");
TEST_EVAL(L"(define compose\n"
	  L"  (lambda (f g)\n"
	  L"    (lambda args\n"
	  L"      (f (apply g args)))))\n"
	  L"((compose - *) 12 75)",	L"-900");

/* from r6rs */
TEST_EVAL(L"(apply + (list 3 4))",		L"7");
//TEST_EVAL(L"((define compose\n"
//	  L"  (lambda (f g)\n"
//	  L"    lambda args\n"
//	  L"      (f (apply g args)))))\n"
//	  L"((compose sqrt *) 12 75)",	L"30");



DEFINE_BLOCK(b_continue_callcc)
{
    obj_t *value = pair_car(F_SUBJ);
    FRAME = F_ENV;
    RETURN(value);
}

DEFINE_PROC(L"call-with-current-continuation")
{
    obj_t *closure = make_C_procedure(b_continue_callcc, NIL, FRAME);
    obj_t *args = make_pair(closure, NIL);
    obj_t *proc = pair_car(F_SUBJ);
    return eval_application(proc, args);
}

ALIAS_NAME(&current_library_, L"call-with-current-continuation",
	   &current_library_, L"call/cc");

//TEST_EVAL(L"(call-with-current-continuation\n"
//          L"  (lambda (exit)\n"
//          L"    (for-each (lambda (x)\n"
//          L"                (if (negative? x)\n"
//          L"                    (exit x)))\n"
//          L"              '(54 0 37 -3 245 19))\n"
//          L"    #t))", L"-3");

//TEST_EVAL(L"(define list-length\n"
//          L"  (lambda (obj)\n"
//          L"    (call-with-current-continuation\n"
//          L"      (lambda (return)\n"
//          L"        (letrec ((r\n"
//          L"                  (lambda (obj)\n"
//          L"                    (cond ((null? obj) 0)\n"
//          L"                          ((pair? obj)\n"
//          L"                           (+ (r (cdr obj)) 1))\n"
//          L"                          (else (return #f))))))\n"
//          L"          (r obj))))))\n"
//          L"(list-length '(1 2 3 4))", L"4");

//TEST_EVAL(L"(list-length '(a b . c))", L"#f");

TEST_EVAL(L"(call-with-current-continuation procedure?)", L"#t");

TEST_EVAL(L"(define plus3 ())\n"
          L"(+ 3 (call/cc\n"
          L"      (lambda (exit)\n"
          L"        (set! plus3 exit)\n"
          L"        4)))\n"
          L"(plus3 5)",			L"8");


/* 11.16.  Iteration
 *
 * ...
 */

/* 11.17.  Quasiquotation
 *
 * ...
 */

/* 11.18.  Binding constructs for syntactic keywords
 *
 * ...
 */

/* 11.19. Macro transformers
 *
 * ...
 */
