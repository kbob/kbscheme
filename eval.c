#include "eval.h"

#include <assert.h>

#include "env.h"
#include "expand.h"
#include "proc.h"
#include "roots.h"
#include "test.h"

#define EVAL_TRACE 0
#if EVAL_TRACE
    #include <stdio.h>
    #include "uprintf.h"
#endif

THREAD_EXTERN_ROOT(FRAME);

DECLARE_EXTERN_BLOCK(b_accum_operator);
DECLARE_BLOCK(b_accum_arg);
DECLARE_BLOCK(b_eval_sequence);

static bool is_self_evaluating(obj_t *expr)
{
    return (is_boolean(expr) ||
	    is_fixnum(expr) ||
	    is_character(expr) ||
	    is_string(expr) ||
	    is_vector(expr) ||
	    is_bytevector(expr));
}

TEST_EVAL(L"#t", L"#t");
TEST_EVAL(L"#f", L"#f");
TEST_EVAL(L"123", L"123");
TEST_EVAL(L"#\\A", L"#\\A");
TEST_EVAL(L"\"ABC\"", L"\"ABC\"");

static bool is_application(obj_t *expr)
{
    return is_pair(expr);
}

static obj_t *eval_symbol(void)
{
    obj_t *binding = env_lookup(F_ENV, F_SUBJ);
    return binding_value(binding);
}

#if EVAL_TRACE

    static const wchar_t *block_name(C_procedure_t *block, obj_t *env)
    {
	if (block == b_eval)
	    return L"b_eval";
	if (block == b_accum_operator)
	    return L"b_accum_operator";
	if (block == b_accum_arg)
	    return L"b_accum_arg";
	if (block == b_eval_sequence)
	    return L"b_eval_sequence";
	if (block == NULL)
	    return L"NULL";
	/* XXX Move this code into env.c. */
	if (!env)
	    env = library_env(r6rs_library());
	if (is_pair(env)) {
	    obj_t *frame = pair_car(env);
	    while (frame) {
		obj_t *binding = pair_car(frame);
		obj_t *value = binding_value(binding);
		if (is_procedure(value) && procedure_is_C(value)) {
		    C_procedure_t *body;
		    body = (C_procedure_t *)procedure_body(value);
		    if (body == block) {
			obj_t *name = symbol_name(binding_name(binding));
			return string_value(name);
		    }
		}
		frame = pair_cdr(frame);
	    }
	}
	return L"<some-proc>";
    }

    void print_stack(const char *label)
    {
	printf("%s: stack = ", label);
	const char *sep = "";
	obj_t *fp;
	for (fp = FRAME; fp; fp = frame_get_parent(fp), sep = " -> ") {
	    C_procedure_t *cont = frame_get_continuation(fp);
	    obj_t *subj = frame_get_subject(fp);
	    printf("%s%ls", sep, block_name(cont, NIL));
	    if (cont || subj)
		printf_unchecked("[%O]", subj);
	} 
	printf("\n");
    }

    void print_env(obj_t *env)
    {
	if (!is_pair(env)) {
	    printf_unchecked("%O\n", env);
	    return;
	}
	const char *sep = "";
	while (env) {
	    printf("%s", sep);
	    if (pair_cdr(env)) {
		obj_t *f = pair_car(env);
		printf("[");
		sep = "";
		while (f) {
		    obj_t *binding = pair_car(f);
		    printf_unchecked("%s%O: %O", sep,
						 binding_name(binding),
						 binding_value(binding));
		    f = pair_cdr(f);
		    sep = ", ";
		}
		printf("]");
	    } else
		printf("[builtins]\n");
	    env = pair_cdr(env);
	    sep = " -> ";
	}
    }

#endif /* EVAL_TRACE */

obj_t *apply_procedure(obj_t *proc, obj_t *args)
{
    PUSH_ROOT(proc);
    PUSH_ROOT(args);
    AUTO_ROOT(body, procedure_body(proc));
    if (procedure_is_C(proc)) {
	obj_t *env = F_ENV;
	if (!procedure_is_special_form(proc))
	    env = procedure_env(proc);
	GOTO_FRAME(make_short_frame, (C_procedure_t *)body, args, env);
    }
    AUTO_ROOT(new_env, make_env(procedure_env(proc)));
    AUTO_ROOT(formals, procedure_args(proc));
    AUTO_ROOT(actuals, args);
    while (!is_null(formals) || !is_null(actuals)) {
	if (is_null(formals))
	    RAISE("too many args");
	obj_t *formal, *actual;
	if (is_pair(formals)) {
	    if (is_null(actuals))
		RAISE("not enough args");
	    formal  = pair_car(formals);
	    formals = pair_cdr(formals);
	    actual  = pair_car(actuals);
	    actuals = pair_cdr(actuals);
	} else {
	    formal  = formals;
	    actual  = actuals;
	    formals = actuals = NIL;
	}
	env_bind(new_env, formal, BT_LEXICAL, M_MUTABLE, actual);
    }
    GOTO(b_eval_sequence, body, new_env);
}

DEFINE_EXTERN_BLOCK(b_eval)
{
    assert(!is_null(F_SUBJ));
    if (is_self_evaluating(F_SUBJ))
	RETURN(F_SUBJ);
    if (is_symbol(F_SUBJ))
	RETURN(eval_symbol());
    if (is_application(F_SUBJ)) {
	AUTO_ROOT(proc, pair_car(F_SUBJ));
	EVAL_THEN_GOTO(proc, F_ENV, b_accum_operator, F_SUBJ, F_ENV);

    }
    RAISE(&syntax && "can't eval");
}

DEFINE_EXTERN_BLOCK(b_accum_operator)
{
    obj_t *proc = VALUE;
    obj_t *args = pair_cdr(F_SUBJ);
    assert(is_procedure(proc));
    assert(!(procedure_is_xformer(proc)));
    if (procedure_is_special_form(proc) || is_null(args))
	return apply_procedure(proc, args);
    PUSH_ROOT(proc);
    AUTO_ROOT(first_arg, pair_car(args));
    AUTO_ROOT(rest_args, pair_cdr(args));
    EVAL_THEN_GOTO_FRAME(first_arg, F_ENV,
			 make_long_frame,
			 b_accum_arg, rest_args, F_ENV, proc, NIL, NIL);
}

DEFINE_BLOCK(b_eval_sequence)
{
    AUTO_ROOT(first, pair_car(F_SUBJ));
    AUTO_ROOT(rest, pair_cdr(F_SUBJ));
    if (is_null(rest))
	TAIL_EVAL(first);
    EVAL_THEN_GOTO(first, F_ENV, b_eval_sequence, rest, F_ENV);
}

DEFINE_BLOCK(b_accum_arg)
{
    /* append the new evaluated arg to the arglist. */
    obj_t *last_arg = make_pair(VALUE, NIL);
    obj_t *arglist = F_ARGL;
    if (is_null(arglist))
	arglist = last_arg;
    else
	pair_set_cdr(F_LARG, last_arg);
    if (is_null(F_SUBJ))
	return apply_procedure(F_PROC, arglist);
    PUSH_ROOT(last_arg);
    PUSH_ROOT(arglist);
    AUTO_ROOT(next_arg, pair_car(F_SUBJ));
    AUTO_ROOT(rest_args, pair_cdr(F_SUBJ));
    EVAL_THEN_GOTO_FRAME(next_arg, F_ENV,
			 make_long_frame, b_accum_arg,
			 rest_args, F_ENV, F_PROC, arglist, last_arg);
}

obj_t *eval_frame(obj_t *frame)
{
    FRAME = frame;
    obj_t *value = NIL;
    while (FRAME) {
	/* XXX mix in setjmp() and a signal flag here. */
#if EVAL_TRACE
	printf_unchecked("    %ls %.56O\n", block_name(F_CONT, F_ENV), F_SUBJ);
	//print_stack("eval");
	//printf_unchecked("   F_SUBJ => %O\n", F_SUBJ);
	//printf_unchecked("   VALUE => %O\n", value);
	//printf("   ENV => ");
	//print_env(F_ENV);
	//printf("\n");
#endif
	value = (*F_CONT)(value);
    }
    return value;
}

obj_t *eval_expanded(obj_t *expr, env_t *env)
{
    FRAME = NIL;
    FRAME = MAKE_CALL(b_eval, expr, env);
    return eval_frame(FRAME);
}

static obj_t *expand(obj_t *expr, env_t *env)
{
    PUSH_ROOT(expr);
    PUSH_ROOT(env);
    AUTO_ROOT(proc, expander());
    AUTO_ROOT(args, make_pair(env, NIL));
    args = make_pair(expr, args);
    //printf_unchecked("proc = %O\n", proc);
    //printf_unchecked("args = %O\n", args);
    FRAME = NIL;
    FRAME = MAKE_CALL(b_eval, make_boolean(false), env);
    apply_procedure(proc, args);
    POP_FUNCTION_ROOTS();
    return eval_frame(FRAME);
}

obj_t *eval(obj_t *expr, env_t *env)
{
    PUSH_ROOT(expr);
    PUSH_ROOT(env);
    expr = expand(expr, env);
    expr = eval_expanded(expr, env);
    POP_FUNCTION_ROOTS();
    return expr;
}
