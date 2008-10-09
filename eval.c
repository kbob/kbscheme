#include "eval.h"

#include <assert.h>
#include <stdio.h>			/* XXX */
#include <stdlib.h>			/* XXX malloc */

#include "bind.h"
#include "print.h"			/* XXX */
#include "proc.h"

static bool is_self_evaluating(obj_t *expr)
{
    return (is_null(expr) ||
	    is_boolean(expr) ||
	    is_fixnum(expr) ||
	    is_character(expr) ||
	    is_string(expr));
}

eval_frame_t *make_frame(eval_frame_t *parent)
{
    eval_frame_t *FRAME = malloc(sizeof *FRAME);
					/* XXX pretend this is GC'd */
    F_SET_PARENT(parent);
    if (parent) {
	F_SET_CONT(parent->ef_continuation);
	F_SET_EXP(parent->ef_subject);
	F_SET_ENV(parent->ef_environment);
	F_SET_VAL(parent->ef_value);
	F_SET_PROC(parent->ef_procedure);
	F_SET_ARGL(parent->ef_arglist);
	F_SET_NXA(parent->ef_next_arg);
    } else {
	F_SET_CONT(NULL);
	F_SET_EXP(make_null());
	F_SET_ENV(make_null());
	F_SET_VAL(make_null());
	F_SET_PROC(make_null());
	F_SET_ARGL(make_null());
	F_SET_NXA(make_null());
    }
    return FRAME;
}

eval_frame_t *make_long_frame(eval_frame_t *parent,
			      C_procedure_t *continuation,
			      obj_t *subject,
			      env_t *environment,
			      obj_t *procedure,
			      obj_t *arglist,
			      obj_t *last_arg)
{
    eval_frame_t *FRAME = malloc(sizeof *FRAME);
    FRAME->ef_parent	   = parent;
    FRAME->ef_continuation = continuation;
    FRAME->ef_value	   = make_null();
    FRAME->ef_subject	   = subject;
    FRAME->ef_environment  = environment;
    FRAME->ef_procedure	   = procedure;
    FRAME->ef_arglist	   = arglist;
    FRAME->ef_last_arg	   = last_arg;
    FRAME->ef_next_arg	   = make_null(); /* XXX delete */
    return FRAME;
}

eval_frame_t *make_short_frame(eval_frame_t *parent,
			       C_procedure_t *continuation,
			       obj_t *subject,
			       env_t *environment)
{
#if 0
    eval_frame_t *FRAME = malloc(sizeof *FRAME);
    FRAME->ef_parent	   = parent;
    FRAME->ef_continuation = continuation;
    FRAME->ef_value	   = make_null();
    FRAME->ef_subject	   = subject;
    FRAME->ef_environment  = environment;
    FRAME->ef_procedure	   = make_null();
    FRAME->ef_arglist	   = make_null();
    FRAME->ef_last_arg	   = make_null();
    FRAME->ef_next_arg	   = make_null(); /* XXX delete */
    return FRAME;
#else
    obj_t *nil = make_null();
    return make_long_frame(parent, continuation, subject, environment, nil, nil, nil);
#endif
}

static obj_t *eval_symbol(eval_frame_t *FRAME)
{
    binding_t *binding = env_lookup(F_ENV, F_EXP);
    return binding_value(binding);
}

static bool is_application(obj_t *expr)
{
    return is_pair(expr);
}

DECLARE_BLOCK(b_eval);
DECLARE_BLOCK(b_accum_operator);
DECLARE_BLOCK(b_accum_arg);
DECLARE_BLOCK(b_rest_arg);
DECLARE_BLOCK(b_eval_sequence);
DECLARE_BLOCK(b_sequence_continue);	/* XXX delete */

#define MAKE_b_eval_FRAME_	     make_short_frame
#define MAKE_b_accum_operator_FRAME_ make_short_frame
#define MAKE_b_accum_arg_FRAME_	     make_long_frame
#define MAKE_b_eval_sequence_FRAME_  make_short_frame

#define TARGET_FRAME_MAKER(target) \
    MAKE_##target##_FRAME_

#define MAKE_GOTO_FRAME(target, arg1, ...) \
    MAKE_##target##_FRAME_(F_PARENT, target, (arg1), __VA_ARGS__)

#define MAKE_CALL_FRAME(target, ...) \
    MAKE_##target##_FRAME_(FRAME, target, __VA_ARGS__)

#define GOTO_FRAME(make_frame, target, ...) \
    return make_frame(F_PARENT, target, __VA_ARGS__)

#define GOTO(target, ...) \
    GOTO_FRAME(TARGET_FRAME_MAKER(target), target, __VA_ARGS__)

#define CALL_THEN_GOTO(callee, target) \
    do { \
	FRAME = MAKE_GOTO_FRAME target; \
	FRAME = MAKE_CALL_FRAME callee; \
	return FRAME; \
    } while (0)

const char *block_name(C_procedure_t *block)
{
    if (block == b_eval)
	return "b_eval";
    if (block == b_accum_operator)
	return "b_accum_operator";
    if (block == b_accum_arg)
	return "b_accum_arg";
    if (block == b_rest_arg)
	return "b_rest_arg";
    if (block == b_sequence_continue)
	return "b_sequence_continue";
    if (block == NULL)
	return "NULL";
    return "???";
}

void print_stack(const char *label, eval_frame_t *FRAME)
{
    printf("%s: stack = ", label);
    const char *sep = "";
    for ( ; FRAME; FRAME = F_PARENT, sep = " -> ") {
	printf("%s%s", sep, block_name(F_CONT));
	if (F_CONT || F_EXP) {
	    printf("(");
	    princ_stdout(F_EXP);
	    printf(")");
	}
    } 
    printf("\n");
}

/* Begin evaluator proper */

#if OLD_EVAL
eval_frame_t *eval_application(eval_frame_t *FRAME)
{
    F_SET_CONT(b_accum_operator);
    FRAME = make_frame(FRAME);
    F_SET_NXA(pair_cdr(F_EXP));
    F_SET_EXP(pair_car(F_EXP));
    F_SET_CONT(b_eval);
    return FRAME;
}
#else
inline eval_frame_t *eval_application(eval_frame_t *FRAME,
				      obj_t *proc,
				      obj_t *args)
{
    obj_t *body = procedure_body(proc);
    if (procedure_is_C(proc))
	GOTO_FRAME(make_short_frame, (C_procedure_t *)body, args, F_ENV);
    obj_t *new_env = make_env(F_ENV);
    obj_t *formals = procedure_args(proc);
    obj_t *actuals = args;
    obj_t *rest = make_null();		/* XXX use this. */
    while (!is_null(formals) || ! is_null(actuals)) {
	if (is_null(formals)) {
	    rest = actuals;
	    break;
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

/*
 * uses F_EXP and F_ENV.
 */

DEFINE_EXTERN_BLOCK(b_eval)
{
#if OLD_EVAL
    if (is_self_evaluating(F_EXP))
	RETURN(F_EXP);
    if (is_symbol(F_EXP))
	RETURN(eval_symbol(FRAME));
    if (is_application(F_EXP))
	return eval_application(FRAME);
    RAISE(&syntax);
#else
    if (is_self_evaluating(F_EXP))
	RETURN(F_EXP);
    if (is_symbol(F_EXP))
	RETURN(eval_symbol(FRAME));
    if (is_application(F_EXP)) {
	obj_t *proc = pair_car(F_EXP);
	obj_t *args = pair_cdr(F_EXP);
	CALL_THEN_GOTO((b_eval, proc, F_ENV),
		       (b_accum_operator, args, F_ENV));
    }
    RAISE(&syntax);
#endif
}

DEFINE_BLOCK(b_accum_operator)
{
#if OLD_EVAL
    F_SET_PROC(F_VAL);
    F_SET_ARGL(make_null());
    F_SET_NXA(pair_cdr(F_EXP));
    F_SET_ARGT(make_null());
    if (procedure_is_special_form(F_PROC) || is_null(F_NXA)) {
	/* special form or nilary procedure: call now. */
	F_SET_ARGL(F_NXA);
	return call_proc(FRAME);
    } else {
	/* evaluate first argument */
	/* XXX merge code w/ b_accum_arg */
	obj_t *exp;
	F_SET_ARGL(make_null());
	if (is_pair(F_NXA)) {
	    F_SET_CONT(b_accum_arg);
	    exp = pair_car(F_NXA);
	    F_SET_NXA(pair_cdr(F_NXA));
	} else {
	    F_SET_CONT(b_rest_arg);
	    exp = F_NXA;
	    F_SET_NXA(make_null());
	}
	EVAL(exp, F_ENV);
    }
#else
    obj_t *proc = F_VAL;
    obj_t *args = F_SUBJ;
    if (procedure_is_special_form(proc) || is_null(args)) {
	return eval_application(FRAME, proc, args);
    }
    obj_t *first_arg = pair_car(args);
    obj_t *rest_args = pair_cdr(args);
    obj_t *nil = make_null();
    CALL_THEN_GOTO((b_eval, first_arg, F_ENV),
		   (b_accum_arg, rest_args, F_ENV, proc, nil, nil));
#endif
}

eval_frame_t *eval_sequence(eval_frame_t *FRAME) /* XXX delete */
{
    F_SET_EXP(pair_car(F_NXA));
    F_SET_NXA(pair_cdr(F_NXA));
    if (is_null(F_NXA))
	TAIL_EVAL(F_EXP, F_ENV);
    F_SET_CONT(b_sequence_continue);
    EVAL(F_EXP, F_ENV);
}


DEFINE_BLOCK(b_sequence_continue)	/* XXX delete */
{
    return eval_sequence(FRAME);
}

#if !OLD_EVAL
DEFINE_BLOCK(b_eval_sequence)
{
    return 0;
}
#endif

eval_frame_t *call_proc(eval_frame_t *FRAME)
{
    if (procedure_is_C(F_PROC))
	return (*(C_procedure_t *)procedure_body(F_PROC))(FRAME);
    else {
	FRAME = make_frame(FRAME);
	F_SET_NXA(procedure_body(F_PROC));
	F_SET_ENV(make_env(procedure_env(F_PROC)));
	obj_t *formals = procedure_args(F_PROC);
	obj_t *arglist = F_ARGL;
	while (formals) {
	    if (is_pair(formals)) {
		obj_t *formal_name = pair_car(formals);
		obj_t *actual_value = pair_car(arglist);
		env_bind(F_ENV, formal_name, BINDING_MUTABLE, actual_value);
		formals = pair_cdr(formals);
		arglist = pair_cdr(arglist);
	    } else {
		assert(is_symbol(formals));
		env_bind(F_ENV, formals, BINDING_MUTABLE, arglist);
		break;
	    }
	}
	return eval_sequence(FRAME);
    }
}

DEFINE_BLOCK(b_accum_arg)
{
    obj_t *exp;
    obj_t *last_arg = make_pair(F_VAL, make_null());
    if (is_null(F_ARGT)) {
	F_SET_ARGL(last_arg);
    } else {
	pair_set_cdr(F_ARGT, last_arg);
    }
    F_SET_ARGT(last_arg);
    if (is_null(F_NXA))
	return call_proc(FRAME);
    if (is_pair(F_NXA)) {
	exp = pair_car(F_NXA);
	F_SET_NXA(pair_cdr(F_NXA));
    } else {
	F_SET_CONT(b_rest_arg);
	exp = F_NXA;
	F_SET_NXA(make_null());
    }
    EVAL(exp, F_ENV);
}

DEFINE_BLOCK(b_rest_arg)
{
    assert(false && "rest arg not implemented");
}

int stack_depth(eval_frame_t *FRAME)
{
    int n = 0;
    while (FRAME) {
	n++;
	FRAME = F_PARENT;
    }
    return n;
}

obj_t *eval(obj_t *expr, env_t *env)
{
    eval_frame_t *FRAME = make_frame(make_frame(NULL));
    F_SET_CONT(b_eval);
    F_SET_EXP(expr);
    F_SET_ENV(env);
    while (F_CONT) {
	// XXX mix in setjmp() and a signal flag here.
#if 0
	print_stack("eval", FRAME);
	printf("   F_EXP => ");
	print_stdout(F_EXP);
	printf("   F_VAL => ");
	print_stdout(F_VAL);
	printf("\n");
#endif
	FRAME = (*F_CONT)(FRAME);
    }
    return F_VAL;
}
