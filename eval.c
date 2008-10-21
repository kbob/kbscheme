#include "eval.h"

#include <assert.h>

#include "bind.h"
#include "proc.h"
#include "roots.h"

//#define EVAL_TRACE 1
#ifdef EVAL_TRACE
#include <stdio.h>			/* XXX */
#include "print.h"			/* XXX */
#endif

#if !PASS_FRAMES
THREAD_EXTERN_ROOT(FRAME);
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

#if PASS_FRAMES
static obj_t *eval_symbol(eval_frame_t FRAME)
#else
static obj_t *eval_symbol(void)
#endif
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

#if PASS_FRAMES
void print_stack(const char *label, eval_frame_t FRAME)
{
    printf("%s: stack = ", label);
    const char *sep = "";
    for ( ; FRAME.ef_frame; FRAME.ef_frame = F_PARENT, sep = " -> ") {
	printf("%s%s", sep, block_name(F_CONT));
	if (F_CONT || F_SUBJ) {
	    printf("(");
	    princ_stdout(F_SUBJ);
	    printf(")");
	}
    } 
    printf("\n");
}
#else
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
#endif

#endif /* EVAL_TRACE */

#if PASS_FRAMES
inline eval_frame_t eval_application(eval_frame_t FRAME,
				     obj_t *proc,
				     obj_t *args)
{
    AUTO_ROOT(body);
    body = procedure_body(proc);
    if (procedure_is_C(proc))
	GOTO_FRAME(make_short_frame, (C_procedure_t *)body, args, F_ENV);
    AUTO_ROOT(new_env);
    AUTO_ROOT(formals);
    AUTO_ROOT(actuals);
    AUTO_ROOT(rest);
    new_env = make_env(F_ENV);
    formals = procedure_args(proc);
    actuals = args;
    rest = NIL;				/* XXX use this. */
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
#else
inline obj_t *eval_application(obj_t *proc, obj_t *args)
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
#endif

#if PASS_FRAMES
DEFINE_EXTERN_BLOCK(b_eval)
{
    if (is_self_evaluating(F_SUBJ))
	RETURN(F_SUBJ);
    if (is_symbol(F_SUBJ))
	RETURN(eval_symbol(FRAME));
    if (is_application(F_SUBJ)) {
	AUTO_ROOT(proc, pair_car(F_SUBJ));
	AUTO_ROOT(args, pair_cdr(F_SUBJ));
	EVAL_THEN_GOTO(proc, F_ENV, b_accum_operator, args, F_ENV);
    }
    RAISE(&syntax);
}
#else
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
#endif

#if PASS_FRAMES
DEFINE_BLOCK(b_accum_operator)
{
    obj_t *proc = F_VAL;
    obj_t *args = F_SUBJ;
    if (procedure_is_special_form(proc) || is_null(args)) {
#if 0
	FRAME = eval_application(FRAME, proc, args);
	POP_FUNCTION_ROOTS();
	return FRAME;
#else
	return eval_application(FRAME, proc, args);
#endif
    }
    AUTO_ROOT(first_arg);
    AUTO_ROOT(rest_args);
    first_arg = pair_car(args);
    rest_args = pair_cdr(args);
    EVAL_THEN_GOTO_FRAME(first_arg, F_ENV,
			 make_long_frame,
			 b_accum_arg, rest_args, F_ENV, proc, NIL, NIL);
}
#else
DEFINE_BLOCK(b_accum_operator)
{
    obj_t *proc = F_VAL;
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
#endif

DEFINE_BLOCK(b_eval_sequence)
{
    AUTO_ROOT(first, pair_car(F_SUBJ));
    AUTO_ROOT(rest, pair_cdr(F_SUBJ));
    if (is_null(rest))
	GOTO(b_eval, first, F_ENV);	/* Optimize tail recursion. */
    EVAL_THEN_GOTO(first, F_ENV, b_eval_sequence, rest, F_ENV);
}

#if PASS_FRAMES
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
#else
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
	return eval_application(F_PROC, arglist);
    PUSH_ROOT(last_arg);
    PUSH_ROOT(arglist);
    AUTO_ROOT(next_arg, pair_car(F_SUBJ));
    AUTO_ROOT(rest_args, pair_cdr(F_SUBJ));
    EVAL_THEN_GOTO_FRAME(next_arg, F_ENV,
			 make_long_frame, b_accum_arg,
			 rest_args, F_ENV, F_PROC, arglist, last_arg);
}
#endif

#if PASS_FRAMES
obj_t *eval(obj_t *expr, env_t *env)
{
    printf("\n");
    printf("eval\n");
    printf("\n");
    PUSH_ROOT(expr);
    PUSH_ROOT(env);
    eval_frame_t FRAME = { make_short_frame(NIL, NULL, NIL, NIL), NIL };
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
    POP_ROOT(env);
    POP_ROOT(expr);
    return F_VAL;
}
#else
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
	printf("   F_VAL => ");
	print_stdout(value);
	printf("\n");
#endif
	value = (*F_CONT)(value);
    }
    return value;
}
#endif
