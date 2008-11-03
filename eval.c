#include "eval.h"

#include <assert.h>

#include "bind.h"
#include "proc.h"
#include "roots.h"

#define EVAL_TRACE 1
#ifdef EVAL_TRACE
#include <stdio.h>			/* XXX */
#include "print.h"			/* XXX */
#endif

THREAD_EXTERN_ROOT(FRAME);

DECLARE_BLOCK(b_accum_operator);
DECLARE_BLOCK(b_accum_arg);
DECLARE_BLOCK(b_eval_sequence);

static bool is_self_evaluating(obj_t *expr)
{
    return (is_null(expr) ||
	    is_boolean(expr) ||
	    is_fixnum(expr) ||
	    is_character(expr) ||
	    is_string(expr));
}

static bool is_application(obj_t *expr)
{
    return is_pair(expr);
}

static obj_t *eval_symbol(void)
{
    obj_t *binding = env_lookup(F_ENV, F_SUBJ);
    return binding_value(binding);
}

#ifdef EVAL_TRACE

const char *block_name(C_procedure_t *block)
{
    if (block == b_eval)
	return "b_eval";
    if (block == b_accum_operator)
	return "b_accum_operator";
    if (block == b_accum_arg)
	return "b_accum_arg";
    if (block == b_eval_sequence)
	return "b_eval_sequence";
    if (block == NULL)
	return "NULL";
    return "<some-proc>";
}

void print_stack(const char *label)
{
    printf("%s: stack = ", label);
    const char *sep = "";
    obj_t *fp;
    for (fp = FRAME; fp; fp = frame_get_parent(fp), sep = " -> ") {
	C_procedure_t *cont = frame_get_continuation(fp);
	obj_t *subj = frame_get_subject(fp);
	printf("%s%s", sep, block_name(cont));
	if (cont || subj) {
	    printf("(");
	    princ_stdout(subj);
	    printf(")");
	}
    } 
    printf("\n");
}

static void print_env(obj_t *env)
{
    const char *sep = "";
    while (env) {
	printf("%s", sep);
	if (pair_cdr(env)) {
	    obj_t *f = pair_car(env);
	    printf("[");
	    sep = "";
	    while (f) {
		obj_t *binding = pair_car(f);
		printf("%s", sep);
		princ_stdout(binding_name(binding));
		printf(": ");
		princ_stdout(binding_value(binding));
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

obj_t *eval_application(obj_t *proc, obj_t *args)
{
    PUSH_ROOT(proc);
    PUSH_ROOT(args);
    AUTO_ROOT(body, procedure_body(proc));
    if (procedure_is_C(proc))
	GOTO_FRAME(make_short_frame, (C_procedure_t *)body, args, F_ENV);
    AUTO_ROOT(new_env, make_env(F_ENV));
    AUTO_ROOT(formals, procedure_args(proc));
    AUTO_ROOT(actuals, args);
    AUTO_ROOT(rest, NIL);		/* XXX use this. */
    while (!is_null(formals) || ! is_null(actuals)) {
	if (is_null(formals))
	    RAISE("too many args");
	if (!is_pair(formals)) {
	    rest = actuals;
	}
	if (is_null(actuals))
	    RAISE("not enough args");
	env_bind(new_env,
		 pair_car(formals), BINDING_MUTABLE, pair_car(actuals));
	formals = pair_cdr(formals);
	actuals = pair_cdr(actuals);
    }
    GOTO(b_eval_sequence, body, new_env);
}

DEFINE_EXTERN_BLOCK(b_eval)
{
    if (is_self_evaluating(F_SUBJ))
	RETURN(F_SUBJ);
    if (is_symbol(F_SUBJ))
	RETURN(eval_symbol());
    if (is_application(F_SUBJ)) {
	AUTO_ROOT(proc, pair_car(F_SUBJ));
	AUTO_ROOT(args, pair_cdr(F_SUBJ));
	EVAL_THEN_GOTO(proc, F_ENV, b_accum_operator, args, F_ENV);
    }
    RAISE(&syntax);
}

DEFINE_BLOCK(b_accum_operator)
{
    obj_t *proc = VALUE;
    obj_t *args = F_SUBJ;
    if (procedure_is_special_form(proc) || is_null(args))
	return eval_application(proc, args);
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
    if (is_null(rest)) {
	printf("tail goto\n");
	TAIL_EVAL(first);
    }
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
	return eval_application(F_PROC, arglist);
    PUSH_ROOT(last_arg);
    PUSH_ROOT(arglist);
    AUTO_ROOT(next_arg, pair_car(F_SUBJ));
    AUTO_ROOT(rest_args, pair_cdr(F_SUBJ));
    EVAL_THEN_GOTO_FRAME(next_arg, F_ENV,
			 make_long_frame, b_accum_arg,
			 rest_args, F_ENV, F_PROC, arglist, last_arg);
}

obj_t *eval(obj_t *expr, env_t *env)
{
    obj_t *value = NIL;
    FRAME = NIL;
    FRAME = MAKE_CALL(b_eval, expr, env);
    while (FRAME) {
	/* XXX mix in setjmp() and a signal flag here. */
#ifdef EVAL_TRACE
	print_stack("eval");
	printf("   F_SUBJ => ");
	print_stdout(F_SUBJ);
	printf("   VALUE => ");
	print_stdout(value);
	printf("   ENV => ");
	print_env(F_ENV);
	printf("\n");
#endif
	value = (*F_CONT)(value);
    }
    return value;
}
