#include <assert.h>

#include "expand.h"
#include "proc.h"
#include "test.h"

LIBRARY(L"(rnrs base (6))")

/* 11.2.1.  Variable definitions
 *
 * (define <variable> <expression>)	  	# syntax
 * (define <variable>)			  	# syntax
 * (define (<variable> <formals>) body)	  	# syntax
 * (define (<variable> . <formals>) body) 	# syntax
 */

DEFINE_BLOCK(b_define_continue)
{
    obj_t *var = pair_car(F_SUBJ);
    env_bind(F_ENV, var, BT_CORE, M_MUTABLE, VALUE);
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
    env_bind(F_ENV, var, BT_LEXICAL, M_MUTABLE, value);
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
 * (define-syntax <keyword> <expression>) 	# syntax
 */

DEFINE_BLOCK(b_define_syntax_continue)
{
    obj_t *proc = VALUE;
    proc = make_xformer_proc(procedure_body(proc),
			     procedure_args(proc),
			     procedure_env(proc));
    obj_t *keyword = pair_car(F_SUBJ);
    env_bind(F_ENV, keyword, BT_MACRO, M_IMMUTABLE, proc);
    RETURN(UNSPECIFIED);
}

DEFINE_SPECIAL_FORM(L"define-syntax")
{
    AUTO_ROOT(exp, pair_cadr(F_SUBJ));
    EVAL_THEN_GOTO(exp, F_ENV, b_define_syntax_continue, F_SUBJ, F_ENV);
}

#if 0
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
#endif

/* 11.4.1.  Quotation
 *
 * (quote <datum>)				# syntax
 */

DEFINE_SPECIAL_FORM(L"quote")
{
    RETURN(pair_car(F_SUBJ));
}

TEST_EVAL(L"(quote ())",		L"()");
TEST_EVAL(L"(quote (a b c))",		L"(a b c)");

/* from r6rs */
TEST_EVAL(L"(quote a)",			L"a");
TEST_EVAL(L"(quote #(a b c))",		L"#(a b c)");
TEST_EVAL(L"(quote (+ 1 2))",		L"(+ 1 2)");

TEST_EVAL(L"'\"abc\"",			L"\"abc\"");
TEST_EVAL(L"'145932",			L"145932");
TEST_EVAL(L"'a",			L"a");
TEST_EVAL(L"'#(a b c)",			L"#(a b c)");
TEST_EVAL(L"'()",			L"()");
TEST_EVAL(L"'(+ 1 2)",			L"(+ 1 2)");
TEST_EVAL(L"'(quote a)",		L"(quote a)");
TEST_EVAL(L"''a",			L"(quote a)");


/* 11.4.2.  Procedures
 *
 * (lambda <formals> <body>)			# syntax
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


TEST_EVAL(L"((lambda x x) 3 4 5 6)",		L"(3 4 5 6)");
TEST_EVAL(L"((lambda (x y . z) z)\n"
          L" 3 4 5 6)",				L"(5 6)");

/* 11.4.3.  Conditionals
 *
 * (if <test> <consequent> <alternate>) 	# syntax
 * (if <test> <consequent>)             	# syntax
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
 * (set! <variable> <expression>)		# syntax
 */

DEFINE_BLOCK(b_set_continue)
{
    obj_t *var = F_SUBJ;
    obj_t *binding = env_lookup(F_ENV, var);
    assert(binding_is_mutable(binding));
    binding_set_value(binding, VALUE);
    RETURN(UNSPECIFIED);
}

DEFINE_SPECIAL_FORM(L"set!")
{
    AUTO_ROOT(var, pair_car(F_SUBJ));
    AUTO_ROOT(exp, pair_cadr(F_SUBJ));
    EVAL_THEN_GOTO(exp, F_ENV, b_set_continue, var, F_ENV);
}

TEST_EVAL(L"(define v1 '()) (set! v1 4) v1", L"4");
TEST_EVAL(L"(define v2 '()) (set! v2 4)",    UNSPECIFIED_REPR);

/* from r6rs */
//TEST_EVAL(L"(let ((x 2))\n"
//          L"  (+ x 1)\n"
//          L"  (set! x 4)\n"
//          L"  (+ x 1))",        L"5");
          
/* 11.4.5.  Derived conditionals
 *
 * (cond <cond clause1> <cond clause2> ...)     # syntax
 * => auxiliary syntax
 * => auxiliary syntax
 *
 * (case <key> <case clause 1> <case clause 2> ...) # syntax
 *
 * (and <test1> ...)				# syntax
 *
 * (or <test1> ...)				# syntax
 */

/* 11.4.6.  Binding constructs
 *
 * (let <bindings> <body>)			# syntax
 *
 * (let* <bindings> <body>)			# syntax
 *
 * (letrec <bindings> <body>)			# syntax
 *
 * (letrec* <bindings> <body>)			# syntax
 *
 * (let-values <mv-bindings> <body>)		# syntax
 *
 * (let*-values <mv-bindings> <body>)		# syntax
 */

#include <stdio.h>			/* XXX */

static obj_t *let_get_args(obj_t *bindings)
{
    PUSH_ROOT(bindings);
    AUTO_ROOT(args, NIL);
    AUTO_ROOT(last_arg, NIL);
    AUTO_ROOT(t, NIL);
    while (!is_null(bindings)) {
	obj_t *var = pair_caar(bindings);
	if (args) {
	    t = make_pair(var, NIL);
	    pair_set_cdr(last_arg, t);
	    last_arg = t;
	}
	else
	    args = last_arg = make_pair(var, NIL);
	bindings = pair_cdr(bindings);
    }
    POP_FUNCTION_ROOTS();
    return args;
}

static obj_t *let_get_inits(obj_t *bindings)
{
    PUSH_ROOT(bindings);
    AUTO_ROOT(inits, NIL);
    AUTO_ROOT(last_init, NIL);
    AUTO_ROOT(t, NIL);
    while (!is_null(bindings)) {
	obj_t *init = pair_cadar(bindings);
	if (inits) {
	    t = make_pair(init, NIL);
	    pair_set_cdr(last_init, t);
	    last_init = t;
	}
	else
	    inits = last_init = make_pair(init, NIL);
	bindings = pair_cdr(bindings);
    }
    POP_FUNCTION_ROOTS();
    return inits;
}

DEFINE_TRANSFORMER(L"lett")
{
    /*
     * (let				((lambda (v1 ...)
     *   ((v1 i1) ...)		=>	  body1 ...)
     *    body1 body2 ...)		 i1 ...)
     *
     * (let				((lambda (var)
     *   var				  (var i1 ...))
     *   ((v1 i1) ...)		=>	 (lambda (v1 ...) body1 ...)
     *   body1 ...)			
     *					
     */
    AUTO_ROOT(let, NIL);
    if (is_pair(pair_cadr(VALUE))) {
	/* unnamed let */
	AUTO_ROOT(args, let_get_args(pair_cadr(VALUE)));
	AUTO_ROOT(inits, let_get_inits(pair_cadr(VALUE)));
	AUTO_ROOT(bodies, pair_cddr(VALUE));
	AUTO_ROOT(tmp, make_pair(args, bodies));
	let = make_symbol_from_C_str(L"lambda");
	let = make_pair(let, tmp);
	let = make_pair(let, inits);
    } else {
	/* named let */
	assert(is_symbol(pair_cadr(VALUE)));
	assert(false && "implement named let");
    }
    POP_FUNCTION_ROOTS();
    return let;
}

static obj_t *list_reverse(obj_t *list)
{
    PUSH_ROOT(list);
    AUTO_ROOT(reversed, NIL);
    while (!is_null(list)) {
	reversed = make_pair(pair_car(list), reversed);
	list = pair_cdr(list);
    }
    POP_ROOT(reversed);
    POP_ROOT(list);
    return reversed;
}

DEFINE_BLOCK(b_letrecstar_continue)
{
    obj_t *var = F_SUBJ;
    obj_t *binding = env_lookup(F_ENV, var);
    assert(binding_is_mutable(binding));
    binding_set_value(binding, VALUE);
    RETURN(NIL);
}

DECLARE_BLOCK(begin);

DEFINE_SPECIAL_FORM(L"letrec*")
{
    // push env
    // put vars in env, initialized to NIL
    // push a bunch of work to eval the inits
    // continue with the body
    // eval arg returns to 
    AUTO_ROOT(env, make_env(F_ENV));
    AUTO_ROOT(bindings, list_reverse(pair_car(F_SUBJ)));
    AUTO_ROOT(cont, make_short_frame(F_PARENT, begin, pair_cdr(F_SUBJ), env));
    AUTO_ROOT(var, NIL);
    while (!is_null(bindings)) {
	var = pair_caar(bindings);
	env_bind(env, var, BT_LEXICAL, M_MUTABLE, NIL);
	cont = make_short_frame(cont, b_letrecstar_continue, var, env);
	cont = make_short_frame(cont, b_eval, pair_cadar(bindings), env);
	bindings = pair_cdr(bindings);
    }
    POP_FUNCTION_ROOTS();
    FRAME = cont;
    return NIL;
}

TEST_EVAL(L"(letrec* () #f)",			L"#f");
TEST_EVAL(L"(letrec* ((var 42)) var)",		L"42");

/* from r6rs */
TEST_EVAL(L"(letrec* ((p\n"
          L"	   (lambda (x)\n"
          L"             (+ 1 (q (- x 1)))))\n"
          L"          (q\n"
          L"           (lambda (y)\n"
          L"             (if (zero? y)\n"
          L"		 0\n"
          L"		 (+ 1 (p (- y 1))))))\n"
          L"          (x (p 5))\n"
          L"          (y x))\n"
          L"	 y)",				L"5");

/* 11.4.7.  Sequencing
 *
 * (begin <form> ...)			 	# syntax
 * (begin <expression> <expression> ...) 	# syntax
 */

DEFINE_STATIC_SPECIAL_FORM(begin, L"begin")
{
    if (!is_null(pair_cdr(F_SUBJ))) {
	AUTO_ROOT(first, pair_car(F_SUBJ));
	EVAL_THEN_GOTO(first, F_ENV,
		       begin, pair_cdr(F_SUBJ), F_ENV);
    }
    else
	TAIL_EVAL(pair_car(F_SUBJ));
}

TEST_EVAL(L"(begin 0 1)",  L"1");
TEST_EVAL(L"(define x 'one)\n"
	  L"(begin\n"
	  L"  (set! x 'two)\n"
	  L"  x)",				L"two");
TEST_EVAL(L"(begin\n"
	  L"  (define x 'one)\n"
	  L"  (set! x 'two)\n"
	  L"  x)",				L"two");

/* 11.5.  Equivalence
 *
 * (eqv? obj1 obj2)				# procedure
 *
 * (eq? obj1 obj2)				# procedure
 *
 * (equal? obj1 obj2)				# procedure
 */

DEFINE_PROC(L"eqv?")
{
    obj_t *obj1 = pair_car(F_SUBJ);
    obj_t *obj2 = pair_cadr(F_SUBJ);

    if (is_fixnum(obj1) && is_fixnum(obj2))
	RETURN(make_boolean(fixnum_value(obj1) == fixnum_value(obj2)));
    if (is_character(obj1) && is_character(obj2))
	RETURN(make_boolean(character_value(obj1) == character_value(obj2)));
    RETURN(make_boolean(obj1 == obj2));
}

TEST_EVAL(L"(eqv? 'a 'a)",			L"#t");
TEST_EVAL(L"(eqv? 'a 'b)",			L"#f");
TEST_EVAL(L"(eqv? 2 2)",			L"#t");
TEST_EVAL(L"(eqv? '() '())",			L"#t");
TEST_EVAL(L"(eqv? 100000000 100000000)",	L"#t");
TEST_EVAL(L"(eqv? (cons 1 2) (cons 1 2))",	L"#f");
TEST_EVAL(L"(eqv? (lambda () 1)\n"
	  L"      (lambda () 2))",		L"#f");
TEST_EVAL(L"(eqv? #f 'nil)",			L"#f");

DEFINE_PROC(L"eq?")
{
    obj_t *obj1 = pair_car(F_SUBJ);
    obj_t *obj2 = pair_cadr(F_SUBJ);
    RETURN(make_boolean(obj1 == obj2));
}

/* from r6rs */
TEST_EVAL(L"(eq? 'a 'a)",               L"#t");
TEST_EVAL(L"(eq? (list 'a) (list 'a))",	L"#f");
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

/* 11.7.4.  Numerical operations
 *
 * (number? obj)				# procedure
 * (complex? obj)				# procedure
 * (real? obj)					# procedure
 * (rational? obj)				# procedure
 * (integer? obj)				# procedure
 *
 * (real-valued? obj)				# procedure
 * (rational-valued? obj)			# procedure
 * (integer-valued? obj)			# procedure
 *
 * (exact? z)					# procedure
 * (inexact? z)					# procedure
 *
 * (inexact z)					# procedure
 * (exact z)					# procedure
 *
 * (= z1 z2 z3 ...)				# procedure
 * (< z1 z2 z3 ...)				# procedure
 * (> z1 z2 z3 ...)				# procedure
 * (<= z1 z2 z3 ...)				# procedure
 * (>= z1 z2 z3 ...)				# procedure
 *
 * (zero? z)					# procedure
 * (positive? x)				# procedure
 * (negative? x)				# procedure
 * (odd? n)					# procedure
 * (even? n)					# procedure
 * (finite? x)					# procedure
 * (infinite? x)				# procedure
 * (nan? x)					# procedure
 *
 * (max x1 x2 ...)				# procedure
 * (min x1 x2 ...)				# procedure
 *
 * (+ z1 ...)					# procedure
 * (* z1 ...)					# procedure
 *
 * (- z)					# procedure
 * (- z1 z2 ...)				# procedure
 *
 * (/ z)					# procedure
 * (/ z1 z2 ...)				# procedure
 *
 * (abs x)					# procedure
 *
 * (div-and-mod x1 x2)				# procedure
 * (div x1 x2)					# procedure
 * (mod x1 x2)					# procedure
 * (div0-and-mod0 x1 x2)			# procedure
 * (div0 x1 x2)					# procedure
 * (mod0 x1 x2)					# procedure
 *
 * (gcd n1 ...)					# procedure
 * (lcm n1 ...)					# procedure
 *
 * (numerator q)				# procedure
 * (denominator q)				# procedure
 *
 * (floor x)					# procedure
 * (ceiling x)					# procedure
 * (truncate x)					# procedure
 * (round x)					# procedure
 *
 * (rationalize x1 x2)				# procedure
 *
 * (exp z)					# procedure
 * (log z)					# procedure
 * (log z1 z2)					# procedure
 * (sin z)					# procedure
 * (cos z)					# procedure
 * (tan z)					# procedure
 * (asin z)					# procedure
 * (acos z)					# procedure
 * (atan x1 x2)					# procedure
 *
 * (sqrt z)					# procedure
 *
 * (exact-integer-sqrt k)			# procedure
 *
 * (expt z1 z2)					# procedure
 *
 * (make-rectangular x1 x2)			# procedure
 * (make-polar x3 x4)				# procedure
 * (real-part z)				# procedure
 * (imag-part z)				# procedure
 * (magnitude z)				# procedure
 * (angle z)					# procedure
 *
 * (number->string z)				# procedure
 * (number->string z radix)			# procedure
 * (number->string z radix precision)		# procedure
 *
 * (string->number string)			# procedure
 * (string->number string radix)		# procedure
 */

DEFINE_PROC(L"number?")
{
    /* XXX RETURN(make_boolean(is_number(pair_car(F_SUBJ)))); */
    RETURN(make_boolean(is_fixnum(pair_car(F_SUBJ))));
}

DEFINE_PROC(L"integer?")
{
    /* XXX RETURN(make_boolean(is_integer(pair_car(F_SUBJ)))); */
    RETURN(make_boolean(is_fixnum(pair_car(F_SUBJ))));
}

DEFINE_PROC(L"=")
{
    obj_t *p = F_SUBJ;
    int x = fixnum_value(pair_car(p));
    while (!is_null(p)) {
	if (fixnum_value(pair_car(p)) != x)
	    RETURN(make_boolean(false));
	p = pair_cdr(p);
    }
    RETURN(make_boolean(true));
}

DEFINE_PROC(L"<")
{
    obj_t *p = F_SUBJ;
    int x = fixnum_value(pair_car(p));
    p = pair_cdr(p);
    while (!is_null(p)) {
	int y = fixnum_value(pair_car(p));
	if (!(x < y))
	    RETURN(make_boolean(false));
	x = y;
	p = pair_cdr(p);
    }
    RETURN(make_boolean(true));
}

DEFINE_PROC(L">")
{
    obj_t *p = F_SUBJ;
    int x = fixnum_value(pair_car(p));
    p = pair_cdr(p);
    while (!is_null(p)) {
	int y = fixnum_value(pair_car(p));
	if (!(x > y))
	    RETURN(make_boolean(false));
	x = y;
	p = pair_cdr(p);
    }
    RETURN(make_boolean(true));
}

DEFINE_PROC(L"<=")
{
    obj_t *p = F_SUBJ;
    int x = fixnum_value(pair_car(p));
    p = pair_cdr(p);
    while (!is_null(p)) {
	int y = fixnum_value(pair_car(p));
	if (!(x <= y))
	    RETURN(make_boolean(false));
	x = y;
	p = pair_cdr(p);
    }
    RETURN(make_boolean(true));
}

DEFINE_PROC(L">=")
{
    obj_t *p = F_SUBJ;
    int x = fixnum_value(pair_car(p));
    p = pair_cdr(p);
    while (!is_null(p)) {
	int y = fixnum_value(pair_car(p));
	if (!(x >= y))
	    RETURN(make_boolean(false));
	x = y;
	p = pair_cdr(p);
    }
    RETURN(make_boolean(true));
}

DEFINE_PROC(L"negative?")
{
    RETURN(make_boolean(fixnum_value(pair_car(F_SUBJ)) < 0));
}

TEST_EVAL(L"(negative? -1)",			L"#t");
TEST_EVAL(L"(negative? 0)",			L"#f");
TEST_EVAL(L"(negative? +1)",			L"#f");

DEFINE_PROC(L"+")
{
    obj_t *p = F_SUBJ;
    int sum = 0;
    while (!is_null(p)) {
	sum += fixnum_value(pair_car(p));
	p = pair_cdr(p);
    }
    RETURN(make_fixnum(sum));
}

TEST_EVAL(L"+",                   L"#<proc-C>");
TEST_EVAL(L"(+)",                 L"0");
TEST_EVAL(L"(+ 3)",               L"3");
TEST_EVAL(L"(+ 3 4)",             L"7");
TEST_EVAL(L"(+ (+ 1 2) (+ 3 4))", L"10");

DEFINE_PROC(L"-")
{
    obj_t *p = F_SUBJ;
    int diff = fixnum_value(pair_car(p));
    p = pair_cdr(p);
    if (is_null(p))
	RETURN(make_fixnum(-diff));
    while (!is_null(p)) {
	diff -= fixnum_value(pair_car(p));
	p = pair_cdr(p);
    }
    RETURN(make_fixnum(diff));
}

DEFINE_PROC(L"*")
{
    obj_t *p = F_SUBJ;
    int prod = 1;
    while (!is_null(p)) {
	prod *= fixnum_value(pair_car(p));
	p = pair_cdr(p);
    }
    RETURN(make_fixnum(prod));
}

DEFINE_PROC(L"div")
{
    int dividend = fixnum_value(pair_car(F_SUBJ));
    int divisor = fixnum_value(pair_cadr(F_SUBJ));
    assert(is_null(pair_cddr(F_SUBJ)));
    RETURN(make_fixnum(dividend / divisor));
}

DEFINE_PROC(L"mod")
{
    int dividend = fixnum_value(pair_car(F_SUBJ));
    int divisor = fixnum_value(pair_cadr(F_SUBJ));
    assert(is_null(pair_cddr(F_SUBJ)));
    RETURN(make_fixnum(dividend % divisor));
}

DEFINE_PROC(L"abs")
{
    assert(is_null(pair_cdr(F_SUBJ)));
    int x = fixnum_value(pair_car(F_SUBJ));
    RETURN(make_fixnum(x < 0 ? -x : x));
}
/* 11.8.  Booleans
 *
 * (not obj)					# procedure
 *
 * (boolean? obj)				# procedure
 *
 * (boolean=? bool1 bool2 bool3 ...)		# procedure
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
 * (pair? obj)					# procedure
 *
 * (cons obj1 obj2)				# procedure
 *
 *
 * (car pair)					# procedure
 *
 * (cdr pair)					# procedure
 *
 * (caar pair)					# procedure
 * (cadr pair)					# procedure
 *     ...				       	     ...
 * (cdddar pair)				# procedure
 * (cddddr pair)				# procedure
 *
 * (null? obj)					# procedure
 *
 * (list? obj)					# procedure
 *
 * (list obj ...)				# procedure
 *
 * (length list)				# procedure
 *
 * (append list ... obj)			# procedure
 *
 * (reverse list)				# procedure
 *
 * (list-tail list k)				# procedure
 *
 * (list-ref list k)				# procedure
 *
 * (map proc list1 list2 ...)			# procedure
 *
 * (for-each proc list1 list2 ...)		# procedure
 */

DEFINE_PROC(L"pair?")
{
    RETURN(make_boolean(is_pair(pair_car(F_SUBJ))));
}

TEST_EVAL(L"(pair? (quote (1 2)))", L"#t");
TEST_EVAL(L"(pair? 12)",            L"#f");
TEST_EVAL(L"(pair? '())",           L"#f");

/* from r6rs */
TEST_EVAL(L"(pair? '(a . b))",      L"#t");
TEST_EVAL(L"(pair? '(a b c))",      L"#t");
TEST_EVAL(L"(pair? '())",           L"#f");
TEST_EVAL(L"(pair? '#(a b))",       L"#f");

DEFINE_PROC(L"cons")
{
    RETURN(make_pair(pair_car(F_SUBJ),
		     pair_cadr(F_SUBJ)));
}

TEST_EVAL(L"(cons 1 2)",           L"(1 . 2)");
TEST_EVAL(L"(cons '(1 2) '(3 4))", L"((1 2) 3 4)");
TEST_EVAL(L"(cons 1 '())",         L"(1)");

/* from r6rs */
TEST_EVAL(L"(cons 'a '())",        L"(a)");
TEST_EVAL(L"(cons '(a) '(b c d))", L"((a) b c d)");
TEST_EVAL(L"(cons \"a\" '(b c))",  L"(\"a\" b c)");
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
//TEST_EVAL(L"(car '())",            &exception);

DEFINE_PROC(L"cdr")
{
    RETURN(pair_cdar(F_SUBJ));
}

TEST_EVAL(L"(cdr (quote (1 2)))", L"(2)");

/* from r6rs */
TEST_EVAL(L"(cdr '((a) b c d))",   L"(b c d)");
TEST_EVAL(L"(cdr '(1 . 2))",       L"2");
//TEST_EVAL(L"(cdr '())",            &exception);

DEFINE_PROC(L"null?")
{
    RETURN(make_boolean(is_null(pair_car(F_SUBJ))));
}

TEST_EVAL(L"(null? '())", L"#t");
TEST_EVAL(L"(null? +)", L"#f");
TEST_EVAL(L"(null? (quote (1 2)))", L"#f");

/* 11.10.  Symbols
 *
 * (symbol? obj)				# procedure
 *
 * symbol->string symbol)			# procedure
 *
 * (symbol=? symbol1 symbol2 symbol3 ...) 	# procedure
 *
 * (string->symbol string)			# procedure
 */

DEFINE_PROC(L"symbol?")
{
    RETURN(make_boolean(is_symbol(pair_car(F_SUBJ))));
}

/* from r6rs */
TEST_EVAL(L"(symbol? 'foo)",			L"#t");
TEST_EVAL(L"(symbol? (car '(a b)))",		L"#t");
TEST_EVAL(L"(symbol? \"bar\")",			L"#f");
TEST_EVAL(L"(symbol? 'nil)",			L"#t");
TEST_EVAL(L"(symbol? '())",			L"#f");
TEST_EVAL(L"(symbol? #f)",			L"#f");

DEFINE_PROC(L"symbol->string")
{
    RETURN(symbol_name(pair_car(F_SUBJ)));
}

/* from r6rs */
TEST_EVAL(L"(symbol->string 'flying-fish)",	L"\"flying-fish\"");
TEST_EVAL(L"(symbol->string 'Martin)",		L"\"Martin\"");
TEST_EVAL(L"(symbol->string\n"
	  L"  (string->symbol \"Malvina\"))",	L"\"Malvina\"");

DEFINE_PROC(L"symbol=?")
{
    obj_t *sym = pair_car(F_SUBJ);
    obj_t *p;
    for (p = pair_cdr(F_SUBJ); !is_null(p); p = pair_cdr(p))
	if (sym != pair_car(p))
	    RETURN(make_boolean(false));
    RETURN(make_boolean(true));
}

TEST_EVAL(L"(symbol=? 'a 'a)",			L"#t");
TEST_EVAL(L"(symbol=? 'a 'b)",			L"#f");
TEST_EVAL(L"(symbol=? 'a 'a 'a 'b)",		L"#f");

DEFINE_PROC(L"string->symbol")
{
    RETURN(make_symbol(pair_car(F_SUBJ)));
}

TEST_EVAL(L"(symbol? (string->symbol \"a\"))",		L"#t");

/* from r6rs */
TEST_EVAL(L"(eq? 'mISSISSIppi 'mississippi)",	 	L"#f");
TEST_EVAL(L"(string->symbol \"mISSISSIppi\")",	 	L"mISSISSIppi");
TEST_EVAL(L"(eq? 'bitBlt (string->symbol \"bitBlt\"))", L"#t");
TEST_EVAL(L"(eq? 'JollyWog\n"
	  L"     (string->symbol\n"
	  L"       (symbol->string 'JollyWog)))",	L"#t");
TEST_EVAL(L"(string=? \"K. Harper, M.D.\"\n"
	  L"          (symbol->string\n"
          L"            (string->symbol \"K. Harper, M.D.\")))", L"#t");

/* 11.11.  Characters
 *
 * (char? obj)					# procedure
 *
 * (char->integer char)				# procedure
 * (integer->char sv)				# procedure
 *
 * (char=? char1 char2 char3 ...)		# procedure
 * (char<? char1 char2 char3 ...)		# procedure
 * (char>? char1 char2 char3 ...)		# procedure
 * (char<=? char1 char2 char3 ...)		# procedure
 * (char>=? char1 char2 char3 ...)		# procedure
 */

DEFINE_PROC(L"char?")
{
    RETURN(make_boolean(is_character(pair_car(F_SUBJ))));
}

DEFINE_PROC(L"char->integer")
{
    wchar_t wc = character_value(pair_car(F_SUBJ));
    RETURN(make_fixnum(wc));
}

DEFINE_PROC(L"integer->char")
{
    int i = fixnum_value(pair_car(F_SUBJ));
    RETURN(make_character((wchar_t)i));
}

/* from r6rs */
TEST_EVAL(L"(integer->char 32)",	L"#\\space");
TEST_EVAL(L"(char->integer (integer->char 5000))",
					L"5000");

DEFINE_PROC(L"char=?")
{
    wchar_t lc = character_value(pair_car(F_SUBJ));
    obj_t *p;
    for (p = pair_cdr(F_SUBJ); !is_null(p); p = pair_cdr(p)) {
	wchar_t rc = character_value(pair_car(p));
	if (!(lc == rc))
	    RETURN(make_boolean(false));
	lc = rc;
    }
    RETURN(make_boolean(true));
}

DEFINE_PROC(L"char<?")
{
    wchar_t lc = character_value(pair_car(F_SUBJ));
    obj_t *p;
    for (p = pair_cdr(F_SUBJ); !is_null(p); p = pair_cdr(p)) {
	wchar_t rc = character_value(pair_car(p));
	if (!(lc < rc))
	    RETURN(make_boolean(false));
	lc = rc;
    }
    RETURN(make_boolean(true));
}

DEFINE_PROC(L"char>?")
{
    wchar_t lc = character_value(pair_car(F_SUBJ));
    obj_t *p;
    for (p = pair_cdr(F_SUBJ); !is_null(p); p = pair_cdr(p)) {
	wchar_t rc = character_value(pair_car(p));
	if (!(lc > rc))
	    RETURN(make_boolean(false));
	lc = rc;
    }
    RETURN(make_boolean(true));
}

DEFINE_PROC(L"char<=?")
{
    wchar_t lc = character_value(pair_car(F_SUBJ));
    obj_t *p;
    for (p = pair_cdr(F_SUBJ); !is_null(p); p = pair_cdr(p)) {
	wchar_t rc = character_value(pair_car(p));
	if (!(lc <= rc))
	    RETURN(make_boolean(false));
	lc = rc;
    }
    RETURN(make_boolean(true));
}

DEFINE_PROC(L"char>=?")
{
    wchar_t lc = character_value(pair_car(F_SUBJ));
    obj_t *p;
    for (p = pair_cdr(F_SUBJ); !is_null(p); p = pair_cdr(p)) {
	wchar_t rc = character_value(pair_car(p));
	if (!(lc >= rc))
	    RETURN(make_boolean(false));
	lc = rc;
    }
    RETURN(make_boolean(true));
}

TEST_EVAL(L"(char=? #\\b #\\a)",	L"#f");
TEST_EVAL(L"(char=? #\\b #\\b)",	L"#t");
TEST_EVAL(L"(char=? #\\b #\\c)",	L"#f");

TEST_EVAL(L"(char=? #\\a #\\a #\\a)",	L"#t");
TEST_EVAL(L"(char=? #\\a #\\a #\\b)",	L"#f");

TEST_EVAL(L"(char<? #\\b #\\a)",	L"#f");
TEST_EVAL(L"(char<? #\\b #\\b)",	L"#f");
TEST_EVAL(L"(char<? #\\b #\\c)",	L"#t");

TEST_EVAL(L"(char>? #\\b #\\a)",	L"#t");
TEST_EVAL(L"(char>? #\\b #\\b)",	L"#f");
TEST_EVAL(L"(char>? #\\b #\\c)",	L"#f");

TEST_EVAL(L"(char<=? #\\b #\\a)",	L"#f");
TEST_EVAL(L"(char<=? #\\b #\\b)",	L"#t");
TEST_EVAL(L"(char<=? #\\b #\\c)",	L"#t");

TEST_EVAL(L"(char>=? #\\b #\\a)",	L"#t");
TEST_EVAL(L"(char>=? #\\b #\\b)",	L"#t");
TEST_EVAL(L"(char>=? #\\b #\\c)",	L"#f");

/* from r6rs */
TEST_EVAL(L"(char<? #\\z #\\\xdf)",	L"#t");
TEST_EVAL(L"(char<? #\\z #\\Z)",	L"#f");

/* 11.12.  Strings
 *
 * (string? obj)				# procedure
 *
 * (make-string k)				# procedure
 * (make-string k char)				# procedure
 *
 * (string char ...)				# procedure
 *
 * (string-length string)			# procedure
 *
 * (string-ref string k)			# procedure
 *
 * (string=? string1 string2 string3 ...)	# procedure
 *
 * (string<?  string1 string2 string3 ...)	# procedure
 * (string>?  string1 string2 string3 ...)	# procedure
 * (string<=?  string1 string2 string3 ...)	# procedure
 * (string>=?  string1 string2 string3 ...)	# procedure
 *
 * (substring string start end)			# procedure
 *
 * (string-append string ...)			# procedure
 *
 * (string->list string)			# procedure
 * (list->string list)				# procedure
 *
 * (string-for-each proc string1 string2 ...)	# procedure
 *
 * (string-copy string)				# procedure
 */

DEFINE_PROC(L"string?")
{
    RETURN(make_boolean(is_string(pair_car(F_SUBJ))));
}

TEST_EVAL(L"(string? \"foo\")",			L"#t");
TEST_EVAL(L"(string? (car '(\"a\" b)))",	L"#t");
TEST_EVAL(L"(string? 'bar)",			L"#f");
TEST_EVAL(L"(string? '())",			L"#f");
TEST_EVAL(L"(string? #f)",			L"#f");

DEFINE_PROC(L"make-string")
{
    int len = fixnum_value(pair_car(F_SUBJ));
    obj_t *cdr = pair_cdr(F_SUBJ);
    wchar_t wc = cdr ? character_value(pair_car(cdr)) : L'\0';
    RETURN(make_string(len, wc));
}

TEST_EVAL(L"(make-string 3 #\\a)",		L"\"aaa\"");

static size_t list_len(obj_t *list)
{
    size_t len = 0;
    while (!is_null(list)) {
	len++;
	list = pair_cdr(list);
    }
    return len;
}

DEFINE_PROC(L"string")
{
    size_t i, len = list_len(F_SUBJ);
    AUTO_ROOT(p, F_SUBJ);
    obj_t *str = make_string(len, L'\0');
    for (p = F_SUBJ, i = 0; !is_null(p); p = pair_cdr(p), i++) {
	wchar_t wc = character_value(pair_car(p));
	string_set_char(str, i, wc);
    }
    POP_ROOT(p);
    RETURN(str);
}

TEST_EVAL(L"(string #\\a #\\b #\\c)",		L"\"abc\"");
TEST_EVAL(L"(string)",				L"\"\"");

DEFINE_PROC(L"string-length")
{
    RETURN(make_fixnum(string_len(pair_car(F_SUBJ))));
}

TEST_EVAL(L"(string-length \"abc\")",		L"3");

DEFINE_PROC(L"string-ref")
{
    obj_t *str = pair_car(F_SUBJ);
    int index = fixnum_value(pair_cadr(F_SUBJ));
    assert(0 <= index && index < string_len(str));
    wchar_t wc = string_value(str)[index];
    RETURN(make_character(wc));
}

TEST_EVAL(L"(string-ref \"cat\" 2)", L"#\\t");

DEFINE_PROC(L"string=?")
{
    obj_t *str = pair_car(F_SUBJ);
    obj_t *p;
    for (p = pair_cdr(F_SUBJ); !is_null(p); p = pair_cdr(p)) {
	if (!strings_are_equal(str, pair_car(p)))
	    RETURN(make_boolean(false));
	str = pair_car(p);
    }
    RETURN(make_boolean(true));
}

/* from r6rs */
TEST_EVAL(L"(string=? \"Stra\xdf" L"e\" \"Strasse\")", L"#f");

DEFINE_PROC(L"string<?")
{
    obj_t *str = pair_car(F_SUBJ);
    obj_t *p;
    for (p = pair_cdr(F_SUBJ); !is_null(p); p = pair_cdr(p)) {
	if (strings_cmp(str, pair_car(p)) >= 0)
	    RETURN(make_boolean(false));
	str = pair_car(p);
    }
    RETURN(make_boolean(true));
}

TEST_EVAL(L"(string<? \"ab\" \"abc\")",		L"#t");
TEST_EVAL(L"(string<? \"a\" \"b\" \"c\")",	L"#t");
TEST_EVAL(L"(string<? \"b\" \"b\" \"c\")",	L"#f");
TEST_EVAL(L"(string<? \"a\" \"b\" \"b\")",	L"#f");

/* from r6rs */
TEST_EVAL(L"(string<? \"z\" \"\xdf\")",		L"#t");
TEST_EVAL(L"(string<? \"z\" \"zz\")",		L"#t");
TEST_EVAL(L"(string<? \"z\" \"Z\")",		L"#f");

DEFINE_PROC(L"string>?")
{
    obj_t *str = pair_car(F_SUBJ);
    obj_t *p;
    for (p = pair_cdr(F_SUBJ); !is_null(p); p = pair_cdr(p)) {
	if (strings_cmp(str, pair_car(p)) <= 0)
	    RETURN(make_boolean(false));
	str = pair_car(p);
    }
    RETURN(make_boolean(true));
}

TEST_EVAL(L"(string>? \"c\" \"b\" \"a\")",	L"#t");
TEST_EVAL(L"(string>? \"b\" \"b\" \"a\")",	L"#f");
TEST_EVAL(L"(string>? \"c\" \"b\" \"b\")",	L"#f");

DEFINE_PROC(L"string<=?")
{
    obj_t *str = pair_car(F_SUBJ);
    obj_t *p;
    for (p = pair_cdr(F_SUBJ); !is_null(p); p = pair_cdr(p)) {
	if (strings_cmp(str, pair_car(p)) > 0)
	    RETURN(make_boolean(false));
	str = pair_car(p);
    }
    RETURN(make_boolean(true));
}

TEST_EVAL(L"(string<=? \"a\" \"b\" \"c\")",	L"#t");
TEST_EVAL(L"(string<=? \"b\" \"b\" \"c\")",	L"#t");
TEST_EVAL(L"(string<=? \"a\" \"b\" \"a\")",	L"#f");

DEFINE_PROC(L"string>=?")
{
    obj_t *str = pair_car(F_SUBJ);
    obj_t *p;
    for (p = pair_cdr(F_SUBJ); !is_null(p); p = pair_cdr(p)) {
	if (strings_cmp(str, pair_car(p)) < 0)
	    RETURN(make_boolean(false));
	str = pair_car(p);
    }
    RETURN(make_boolean(true));
}

TEST_EVAL(L"(string>=? \"c\" \"b\" \"a\")",	L"#t");
TEST_EVAL(L"(string>=? \"b\" \"b\" \"a\")",	L"#t");
TEST_EVAL(L"(string>=? \"c\" \"b\" \"c\")",	L"#f");

DEFINE_PROC(L"substring")
{
    AUTO_ROOT(str, pair_car(F_SUBJ));
    int start = fixnum_value(pair_cadr(F_SUBJ));
    int end = fixnum_value(pair_caddr(F_SUBJ));
    assert(0 <= start && start <= end && end <= string_len(str));
    obj_t *substr = make_string(end - start, '\0');
    int i;
    for (i = start; i < end; i++)
	string_set_char(substr, i - start, string_value(str)[i]);
    POP_ROOT(str);
    RETURN(substr);
}

TEST_EVAL(L"(substring \"abcde\" 1 3)",		L"\"bc\"");
TEST_EVAL(L"(substring \"abcde\" 1 1)",		L"\"\"");

DEFINE_PROC(L"string-append")
{
    obj_t *p;
    size_t len = 0;
    for (p = F_SUBJ; !is_null(p); p = pair_cdr(p))
	len += string_len(pair_car(p));
    obj_t *str = make_string(len, L'\0');
    size_t pos = 0;
    for (p = F_SUBJ; !is_null(p); p = pair_cdr(p)) {
	obj_t *sub = pair_car(p);
	size_t sub_len = string_len(sub);
	string_set_substring(str, pos, sub_len, string_value(sub));
	pos += sub_len;
    }
    RETURN(str);
}

TEST_EVAL(L"(string-append \"abc\" \"d\" \"ef\")", L"\"abcdef\"");
TEST_EVAL(L"(string-append \"\" \"\" \"\")",	   L"\"\"");

DEFINE_PROC(L"string->list")
{
    AUTO_ROOT(str, pair_car(F_SUBJ));
    AUTO_ROOT(list, NIL);
    AUTO_ROOT(chr, NIL);
    size_t pos;
    for (pos = string_len(str); pos; --pos) {
	chr = make_character(string_value(str)[pos - 1]);
	list = make_pair(chr, list);
    }
    RETURN(list);
}

TEST_EVAL(L"(string->list \"abc\")",	L"(#\\a #\\b #\\c)");

DEFINE_PROC(L"list->string")
{
    size_t len = list_len(pair_car(F_SUBJ));
    obj_t *str = make_string(len, L'\0');
    obj_t *p;
    size_t pos;
    for (p = pair_car(F_SUBJ), pos = 0; !is_null(p); p = pair_cdr(p), pos++)
	string_set_char(str, pos, character_value(pair_car(p)));
    RETURN(str);
}

TEST_EVAL(L"(list->string '(#\\a #\\b #\\c))",	L"\"abc\"");

DEFINE_PROC(L"string-copy")
{
    size_t len = string_len(pair_car(F_SUBJ));
    obj_t *str = make_string(len, L'\0');
    string_set_substring(str, 0, len, string_value(pair_car(F_SUBJ)));
    RETURN(str);
}

TEST_EVAL(L"(define s \"asdf\")\n"
	  L"(define t (string-copy s))\n"
	  L"t",					L"\"asdf\"");
TEST_EVAL(L"(define s \"asdf\")\n"
	  L"(define t (string-copy s))\n"
	  L"(eq? s t)",				L"#f");

/* 11.13.  Vectors
 *
 * (vector? obj)				# procedure
 *
 * (make-vector k)				# procedure
 * (make-vector k fill)				# procedure
 *
 * (vector obj ...)				# procedure
 *
 * (vector-length vector)			# procedure
 *
 * (vector-ref vector k)			# procedure
 *
 * (vector-set! vector k obj)			# procedure
 *
 * (vector->list vector)			# procedure
 * (list->vector list)				# procedure
 * (vector-fill! vector fill)			# procedure
 * (vector-map proc vector1 vector2 ...)	# procedure
 *
 * (vector-for-each proc vector1 vector2 ...)	# procedure
 */

DEFINE_PROC(L"vector?")
{
    RETURN(make_boolean(is_vector(pair_car(F_SUBJ))));
}

TEST_EVAL(L"(vector? '(3 4))",	L"#f");
TEST_EVAL(L"(vector? '())",	L"#f");
TEST_EVAL(L"(vector? '#(3))",	L"#t");

DEFINE_PROC(L"make-vector")
{
    size_t k = fixnum_value(pair_car(F_SUBJ));
    obj_t *fill = NIL;
    if (!is_null(pair_cdr(F_SUBJ)))
	fill = pair_cadr(F_SUBJ);
    RETURN(make_vector(k, fill));
}

TEST_EVAL(L"(make-vector 3)",	L"#(() () ())");
TEST_EVAL(L"(make-vector 3 4)",	L"#(4 4 4)");

#if 0
DEFINE_PROC(L"vector")
{
    int i, n = list_length(F_SUBJ);
    obj_t *vec = make_vector(n, NIL);
    obj_t *p = F_SUBJ;
    for (i = 0; i < n; i++) {
	vector_set(vec, i, pair_car(p));
	p = pair_cdr(p);
    }
    RETURN(vec);
}

TEST_EVAL(L"(vector 'a 'b 'c)",		L"#(a b c)");
#endif

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
TEST_EVAL(L"(vector-ref '#(0 1 2) 2)",	L"2");

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
TEST_EVAL(L"'#(0 (2 2 2 2) \"Anna\")",	L"#(0 (2 2 2 2) \"Anna\")");
//TEST_EVAL(L"(vector 'a 'b 'c)",		L"#(a b c)");
//TEST_EVAL(L"(list ((vec (vector 0 '(2 2 2 2) \"Anna\")))\n"
//	  L"  (vector-set! vec 1 '(\"Sue\" \"Sue\"))\n"
//	  L"  vec)",			L"#(0 (\"Sue\" \"Sue\") \"Anna\")");
//TEST_EVAL(L"(vector-set! '#(0 1 2) 1 \"doe\")\n", L"&assertion");

/* 11.14.  Errors and violations
 *
 * (error who message irritant1 ...)		# procedure
 *
 * (assert <expression>)			# syntax
 */

/* 11.15. Control features
 *
 * (apply proc arg1 ... rest-args)		# procedure
 *
 * (call-with-current-continuation proc)	# procedure
 * (call/cc proc)				# procedure
 *
 * (values obj ...)				# procedure
 *
 * (call-with-values producer consumer		# procedure
 *
 * (dynamic-wind before thunk after)		# procedure
 */

DEFINE_PROC(L"apply")
{
    AUTO_ROOT(proc, pair_car(F_SUBJ));
    AUTO_ROOT(args, pair_cdr(F_SUBJ));
    AUTO_ROOT(arglist, NIL);
    AUTO_ROOT(last_arg, NIL);
    while (!is_null(args)) {
	obj_t *arg;
	if (!is_null(pair_cdr(args)))
	    arg = make_pair(pair_car(args), NIL);
	else
	    arg = pair_car(args);
	if (!is_null(arglist))
	    pair_set_cdr(last_arg, arg);
	else
	    arglist = arg;
	last_arg = arg;
	args = pair_cdr(args);
    }
    POP_FUNCTION_ROOTS();
    return apply_procedure(proc, arglist);
}

TEST_EVAL(L"(apply + '())",		L"0");
TEST_EVAL(L"(apply + '(2))",		L"2");
TEST_EVAL(L"(apply + '(2 3))",		L"5");
TEST_EVAL(L"(apply + 2 '())",		L"2");
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
    return apply_procedure(proc, args);
}

ALIAS_NAME(&current_library_, L"call-with-current-continuation",
	   &current_library_, L"call/cc");

TEST_EVAL(L"(call-with-current-continuation\n"
          L"  (lambda (exit)\n"
          L"    (for-each (lambda (x)\n"
          L"                (if (negative? x)\n"
          L"                    (exit x)))\n"
          L"              '(54 0 37 -3 245 19))\n"
          L"    #t))", L"-3");

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

TEST_EVAL(L"(define plus3 '())\n"
          L"(+ 3 (call/cc\n"
          L"      (lambda (exit)\n"
          L"        (set! plus3 exit)\n"
          L"        4)))\n"
          L"(plus3 5)",			L"8");


/* 11.17.  Quasiquotation
 *
 * (quasiquote <qq-template>)			# syntax
 * unquote					# auxiliary syntax
 * unquote-splicing				# auxiliary syntax
 */

/* 11.18.  Binding constructs for syntactic keywords
 *
 * (let-syntax <bindings> <form> ...)		# syntax
 *
 * (letrec-syntax <bindings> <form> ...)	# syntax
 */

/* 11.19. Macro transformers
 *
 * (syntax-rules (<literal> ...) <syntax rule> ...)
 *						# syntax (expand)
 * _						# auxiliary syntax (expand)
 * ...						# auxiliary syntax (expand)
 *
 * (identifier-syntax <template>)		# syntax (expand)
 * (identifier-syntax				# syntax (expand)
 *   (<id1> <template1>)
 *   ((set! <id2> <pattern)
 *    <template2))
 * set!						# auxiliary syntax (expand)
 */
