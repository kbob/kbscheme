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

#if !NEW_EVAL

static obj_t *eval_symbol(obj_t *expr, obj_t *env)
{
    binding_t *binding = env_lookup(env, expr);
    return binding_value(binding);
}

static obj_t *eval_arglist(obj_t *arglist, env_t *env)
{
    if (is_null(arglist))
	return make_null();
    else if (is_pair(arglist))
	return make_pair(eval(pair_car(arglist), env),
			 eval_arglist(pair_cdr(arglist), env));
    else
	return eval(arglist, env);
}

static obj_t *eval_multi(obj_t *body, env_t *env)
{
    obj_t *value;
    while (!is_null(body)) {
	value = eval(pair_car(body), env);
	body = pair_cdr(body);
    }
    return value;
}

static obj_t *eval_apply(obj_t *proc,
			 obj_t *arglist,
			 env_t *env)
{
    env_t *new_env = make_env(procedure_env(proc));
    obj_t *formals = procedure_args(proc);
    while (formals) {
	if (is_pair(formals)) {
	    obj_t *formal_name = pair_car(formals);
	    obj_t *actual_value = pair_car(arglist);
	    env_bind(new_env, formal_name, BINDING_MUTABLE, actual_value);
	    formals = pair_cdr(formals);
	    arglist = pair_cdr(arglist);
	} else {
	    assert(is_symbol(formals));
	    env_bind(new_env, formals, BINDING_MUTABLE, arglist);
	    break;
	}
    }
    return eval_multi(procedure_code(proc), new_env);
}

static obj_t *eval_procedure(obj_t *proc, obj_t *arglist, env_t *env)
{
    assert(is_procedure(proc));
    if (procedure_is_C(proc)) {
        obj_t *code = procedure_code(proc);
	return (*(obj_t * (*)())code)(arglist, env);
    } else
	return eval_apply(proc, arglist, env);
}

static obj_t *eval_list(obj_t *expr, obj_t *env)
{
    obj_t *proc = pair_car(expr);
    obj_t *arglist = pair_cdr(expr);
    if (is_symbol(proc)) {
	binding_t *binding = env_lookup(env, proc);
	assert(!is_null(binding));
	proc = binding_value(binding);
    } else if (is_pair(proc))
	proc = eval_list(proc, env);
    else
	assert(false && "can't eval");
    if (is_procedure(proc)) {
	if (!procedure_is_special_form(proc))
	    arglist = eval_arglist(arglist, env);
	return eval_procedure(proc, arglist, env);
    }
    assert(false && "can't apply");
}

obj_t *eval(obj_t *expr, obj_t *env)
{
    if (is_self_evaluating(expr))
	return expr;
    if (is_symbol(expr))
	return eval_symbol(expr, env);
    if (is_pair(expr))
	return eval_list(expr, env);
    assert(false && "don't know how to evaluate");
}

bool is_application(obj_t *expr)
{
    return is_pair(expr);
}

#else

eval_frame_t *make_frame(eval_frame_t *parent)
{
    eval_frame_t *FRAME = malloc(sizeof *FRAME);
					/* XXX pretend this is GC'd */
    F_SET_PARENT(parent);
    if (parent) {
	F_SET_CONT(parent->ef_continue);
	F_SET_EXP(parent->ef_expression);
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

// Registers: EXP ENV VAL STATE PROC ARGL UNEV TMP
// New Registers: CONT EXP ENV VAL MORE
// States: START DONE DID_OPERATOR ACCUM_ARG LAST_ARG
// Proc/SF states: RETURN EVAL TAIL_EVAL YIELD RAISE (RAISE_CONTINUABLE?)

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
DECLARE_BLOCK(b_have_operator);
DECLARE_BLOCK(b_next_arg);
DECLARE_BLOCK(b_rest_arg);
DECLARE_BLOCK(b_sequence_continue);

const char *block_name(C_procedure_t *block)
{
    if (block == b_eval)
	return "b_eval";
    if (block == b_have_operator)
	return "b_have_operator";
    if (block == b_next_arg)
	return "b_next_arg";
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

eval_frame_t *eval_application(eval_frame_t *FRAME)
{
    F_SET_CONT(b_have_operator);
    FRAME = make_frame(FRAME);
    F_SET_NXA(pair_cdr(F_EXP));
    F_SET_EXP(pair_car(F_EXP));
    F_SET_CONT(b_eval);
    return FRAME;
}

DEFINE_EXTERN_BLOCK(b_eval)
{
    if (is_self_evaluating(F_EXP))
	RETURN(F_EXP);
    if (is_symbol(F_EXP))
	RETURN(eval_symbol(FRAME));
    if (is_application(F_EXP))
	return eval_application(FRAME);
    RAISE(&syntax);
}

eval_frame_t *eval_sequence(eval_frame_t *FRAME)
{
    F_SET_EXP(pair_car(F_NXA));
    F_SET_NXA(pair_cdr(F_NXA));
    if (is_null(F_NXA))
	TAIL_EVAL(F_EXP, F_ENV);
    F_SET_CONT(b_sequence_continue);
    EVAL(F_EXP, F_ENV);
}

DEFINE_BLOCK(b_sequence_continue)
{
    return eval_sequence(FRAME);
}

eval_frame_t *call_proc(eval_frame_t *FRAME)
{
    if (procedure_is_C(F_PROC))
	return (*(C_procedure_t *)procedure_code(F_PROC))(FRAME);
    else {
	FRAME = make_frame(FRAME);
	F_SET_NXA(procedure_code(F_PROC));
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

DEFINE_BLOCK(b_have_operator)
{
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
	/* XXX merge code w/ b_next_arg */
	obj_t *exp;
	F_SET_ARGL(make_null());
	if (is_pair(F_NXA)) {
	    F_SET_CONT(b_next_arg);
	    exp = pair_car(F_NXA);
	    F_SET_NXA(pair_cdr(F_NXA));
	} else {
	    F_SET_CONT(b_rest_arg);
	    exp = F_NXA;
	    F_SET_NXA(make_null());
	}
	EVAL(exp, F_ENV);
    }
}

DEFINE_BLOCK(b_next_arg)
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

obj_t *eval(obj_t *expr, obj_t *env)
{
    eval_frame_t *FRAME = make_frame(make_frame(NULL));
    F_SET_CONT(b_eval);
    F_SET_EXP(expr);
    F_SET_ENV(env);
    while (F_CONT) {
	// XXX mix in setjmp() and a signal flag here.
#if 1
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

#endif
