#include "eval.h"

#include <assert.h>

#include "bind.h"
#include "proc.h"

#ifdef EVAL_TRACE
#include <stdio.h>			/* XXX */
#include "print.h"			/* XXX */
#endif

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

static obj_t *eval_symbol(eval_frame_t FRAME)
{
    binding_t *binding = env_lookup(F_ENV, F_SUBJ);
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

void print_stack(const char *label, eval_frame_t *FRAME)
{
    printf("%s: stack = ", label);
    const char *sep = "";
    for ( ; FRAME; FRAME = F_PARENT, sep = " -> ") {
	printf("%s%s", sep, block_name(F_CONT));
	if (F_CONT || F_SUBJ) {
	    printf("(");
	    princ_stdout(F_SUBJ);
	    printf(")");
	}
    } 
    printf("\n");
}

#endif /* EVAL_TRACE */

inline eval_frame_t eval_application(eval_frame_t FRAME,
				     obj_t *proc,
				     obj_t *args)
{
    obj_t *body = procedure_body(proc);
    if (procedure_is_C(proc))
	GOTO_FRAME(make_short_frame, (C_procedure_t *)body, args, F_ENV);
    obj_t *new_env = make_env(F_ENV);
    obj_t *formals = procedure_args(proc);
    obj_t *actuals = args;
    obj_t *rest = NIL;			/* XXX use this. */
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
	RETURN(eval_symbol(FRAME));
    if (is_application(F_SUBJ)) {
	obj_t *proc = pair_car(F_SUBJ);
	obj_t *args = pair_cdr(F_SUBJ);
	EVAL_THEN_GOTO(proc, F_ENV, b_accum_operator, args, F_ENV);
    }
    RAISE(&syntax);
}

DEFINE_BLOCK(b_accum_operator)
{
    obj_t *proc = F_VAL;
    obj_t *args = F_SUBJ;
    if (procedure_is_special_form(proc) || is_null(args)) {
	return eval_application(FRAME, proc, args);
    }
    obj_t *first_arg = pair_car(args);
    obj_t *rest_args = pair_cdr(args);
#if 0
    // XXX use EVAL_THEN_GOTO
    CALL_THEN_GOTO_FRAME((b_eval, first_arg, F_ENV),
			 (make_long_frame,
			  b_accum_arg, rest_args, F_ENV, proc, NIL, NIL));
#else
    EVAL_THEN_GOTO_FRAME(first_arg, F_ENV,
			 make_long_frame,
			 b_accum_arg, rest_args, F_ENV, proc, NIL, NIL);
#endif
}

DEFINE_BLOCK(b_eval_sequence)
{
    obj_t *first = pair_car(F_SUBJ);
    obj_t *rest = pair_cdr(F_SUBJ);
    if (is_null(rest))
	GOTO(b_eval, first, F_ENV);	/* Optimize tail recursion. */
    EVAL_THEN_GOTO(first, F_ENV, b_eval_sequence, rest, F_ENV);
}

DEFINE_BLOCK(b_accum_arg)
{
    /* append the new evaluated arg to the arglist. */
    obj_t *last_arg = make_pair(F_VAL, NIL);
    obj_t *arglist = F_ARGL;
    if (is_null(arglist))
	arglist = last_arg;
    else
	pair_set_cdr(F_LARG, last_arg);

    if (is_null(F_SUBJ))
	return eval_application(FRAME, F_PROC, arglist);
    obj_t *next_arg = pair_car(F_SUBJ);
    obj_t *rest_args = pair_cdr(F_SUBJ);
    EVAL_THEN_GOTO_FRAME(next_arg, F_ENV,
			 make_long_frame, b_accum_arg,
			 rest_args, F_ENV, F_PROC, arglist, last_arg);
}

obj_t *eval(obj_t *expr, env_t *env)
{
    eval_frame_t FRAME = { make_short_frame(NIL, NULL, NIL, NIL) };
    FRAME = MAKE_CALL(b_eval, expr, env);
    while (F_CONT) {
	/* XXX mix in setjmp() and a signal flag here. */
#ifdef EVAL_TRACE
	print_stack("eval", FRAME);
	printf("   F_SUBJ => ");
	print_stdout(F_SUBJ);
	printf("   F_VAL => ");
	print_stdout(F_VAL);
	printf("\n");
#endif
	FRAME = (*F_CONT)(FRAME);
    }
    return F_VAL;
}
