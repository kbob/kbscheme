#include <assert.h>

#include "proc.h"

/* 11.1.1.  Base types
 *
 *    boolean?		pair?
 *    symbol?		number?
 *    char?		string?
 *    vector?		procedure?
 *    null?
 */

DEFINE_PROC(L"boolean?")
{
    RETURN(make_boolean(is_boolean(pair_car(F_SUBJ))));
}

DEFINE_PROC(L"symbol?")
{
    RETURN(make_boolean(is_symbol(pair_car(F_SUBJ))));
}

DEFINE_PROC(L"char?")
{
    RETURN(make_boolean(is_character(pair_car(F_SUBJ))));
}

DEFINE_PROC(L"vector?")
{
    /* RETURN(make_boolean(is_vector(pair_car(F_SUBJ)))); */
    RETURN(make_boolean(false));
}

DEFINE_PROC(L"number?")
{
    /* RETURN(make_boolean(is_number(pair_car(F_SUBJ)))); */
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
    RETURN(NIL);
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
	value = NIL;
    } else {
	assert(is_null(pair_cdr(rest)));
	AUTO_ROOT(exp, pair_car(rest));
	EVAL_THEN_GOTO(exp, F_ENV, b_define_continue, F_SUBJ, F_ENV);
    }
    env_bind(F_ENV, var, BINDING_MUTABLE, value);
    RETURN(NIL);
}

/* 11.2.2.  Syntax definitions
 *
 * (define-syntax <keyword> <expression>) # syntax
 */

/* 11.4.1.  Quotation
 *
 * (quote <datum>)			# syntax
 */

DEFINE_SPECIAL_FORM(L"quote")
{
    assert(is_null(pair_cdr(F_SUBJ)));
    RETURN(pair_car(F_SUBJ));
}

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
	RETURN(cddr);
    obj_t *alternate = pair_car(cddr);
    TAIL_EVAL(alternate);
}

DEFINE_SPECIAL_FORM(L"if")
{
    AUTO_ROOT(test, pair_car(F_SUBJ));
    AUTO_ROOT(subj_cdr, pair_cdr(F_SUBJ));
    EVAL_THEN_GOTO(test, F_ENV, b_continue_if, subj_cdr, F_ENV);
}

/* 11.4.4.  Assignments
 *
 * (set! <variable> <expression>)	# syntax
 */

DEFINE_BLOCK(b_set_continue)
{
    obj_t *var = F_SUBJ;
    obj_t *binding = env_lookup(F_ENV, var);
    assert(binding_is_mutable(binding));
    env_bind(F_ENV, var, BINDING_MUTABLE, VALUE);
    RETURN(NIL);
}

DEFINE_SPECIAL_FORM(L"set!")
{
    AUTO_ROOT(var, pair_car(F_SUBJ));
    AUTO_ROOT(exp, pair_car(pair_cdr(F_SUBJ)));
    EVAL_THEN_GOTO(exp, F_ENV, b_set_continue, var, F_ENV);
}

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
    obj_t *obj2 = pair_car(pair_cdr(F_SUBJ));
    RETURN(make_boolean(obj1 == obj2));
}

/* 11.6.  Procedure predicate
 *
 * (procedure? obj)
 */

DEFINE_PROC(L"procedure?")
{
    RETURN(make_boolean(is_procedure(pair_car(F_SUBJ))));
}

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

DEFINE_PROC(L"cons")
{
    RETURN(make_pair(pair_car(F_SUBJ),
		     pair_car(pair_cdr(F_SUBJ))));
}

DEFINE_PROC(L"car")
{
    RETURN(pair_car(pair_car(F_SUBJ)));
}

DEFINE_PROC(L"cdr")
{
    RETURN(pair_cdr(pair_car(F_SUBJ)));
}

DEFINE_PROC(L"null?")
{
    RETURN(make_boolean(is_null(pair_car(F_SUBJ))));
}

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
 * ...
 */

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

#if 0
DEFINE_PROC(L"call-with-current-continuation")
{
    obj_t *args = make_pair(FRAME, NIL);
    obj_t *proc = pair_car(F_SUBJ);
    return eval_application(FRAME, proc, args);
}
REBIND_PROC(L"call-with-current-continuation", L"call/cc");
#endif

/* XXX How can I bind call/cc to call-with-current-continuation? */

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
