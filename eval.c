#include "eval.h"

#include <assert.h>
#include <stdio.h>			/* XXX */
#include <stdlib.h>			/* XXX malloc */

#include "bind.h"
#include "print.h"			/* XXX */

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
#endif

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

#if !NEW_EVAL

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

// Cheat: create continuations w/ malloc() but pretend they're
// collected.  Then they can be C structs.

typedef enum eval_state {
    ES_DONE,
    ES_DID_OPERATOR,
    ES_ACCUM_ARG,
    ES_ACCUM_LAST_ARG,
} eval_state_t;

enum prim_state {
    PS_RETURN,
    PS_EVAL,
    PS_TAIL_EVAL,
    PS_YIELD,
    PS_RAISE,
    PS_RAISE_CONTINUABLE
};

struct continuation {
    continuation_t *c_parent;
    eval_state_t    c_state;
    obj_t          *c_exp;
    obj_t          *c_env;
    obj_t          *c_val;
    obj_t          *c_proc;
    obj_t          *c_argl;
    obj_t          *c_nxarg;
    obj_t          *c_tmp;
};

#define C_STATE        (cont->c_state)
#define C_EXP          (cont->c_exp)
#define C_ENV          (cont->c_env)
#define C_VAL          (cont->c_val)
#define C_PROC         (cont->c_proc)
#define C_ARGL         (cont->c_argl)
#define C_NXARG        (cont->c_nxarg)
#define C_TMP          (cont->c_tmp)

#define C_SET_STATE(x) (cont->c_state = (x))
#define C_SET_EXP(x)   (cont->c_exp = (x))
#define C_SET_ENV(x)   (cont->c_env = (x))
#define C_SET_VAL(x)   (cont->c_val = (x))
#define C_SET_PROC(x)  (cont->c_proc = (x))
#define C_SET_ARGL(x)  (cont->c_argl = (x))
#define C_SET_NXARG(x) (cont->c_nxarg = (x))
#define C_SET_TMP(x)   (cont->c_tmp = (x))

#define RAISE(condition) (printf("error at line %d\n", __LINE__), exit(1))

continuation_t *make_continuation(continuation_t *parent)
{
    continuation_t *cont = malloc(sizeof *cont); /* XXX pretend this is GC'd */
    cont->c_parent = parent;
    if (parent) {
	C_SET_STATE(parent->c_state);
	C_SET_EXP(parent->c_exp);
	C_SET_ENV(parent->c_env);
	C_SET_VAL(parent->c_val);
	C_SET_PROC(parent->c_proc);
	C_SET_ARGL(parent->c_argl);
	C_SET_NXARG(parent->c_nxarg);
	C_SET_TMP(parent->c_tmp);
    } else {
	C_SET_STATE(ES_DONE);
	C_SET_EXP(make_null());
	C_SET_ENV(make_null());
	C_SET_VAL(make_null());
	C_SET_PROC(make_null());
	C_SET_ARGL(make_null());
	C_SET_NXARG(make_null());
	C_SET_TMP(make_null());
    }
    return cont;
}

// Registers: EXP ENV VAL STATE PROC ARGL UNEV TMP
// States: START DONE DID_OPERATOR ACCUM_ARG LAST_ARG
// Proc/SF states: RETURN EVAL TAIL_EVAL YIELD RAISE (RAISE_CONTINUABLE?)

// How does TMP interact with call/cc?

static obj_t *eval_symbol(continuation_t *cont)
{
    binding_t *binding = env_lookup(C_ENV, C_EXP);
    return binding_value(binding);
}

static obj_t *eval_application(continuation_t **cont)
{
    
}

obj_t *eval(obj_t *expr, obj_t *env)
{
    continuation_t *cont = make_continuation(NULL);
    obj_t *val;
    C_SET_EXP(expr);
    C_SET_ENV(env);
    while (true) {
	printf("C_EXP => ");
	princ(C_EXP, make_file_outstream(stdout));
	printf("\n");
	if (is_self_evaluating(C_EXP))
	    val = C_EXP;
	else if (is_symbol(C_EXP))
	    val = eval_symbol(cont);
	else if (is_application(C_EXP)) {
	    
	} else
	    RAISE(&syntax);

	switch (C_STATE) {
	default:
	    assert(false && "unknown eval state");

	case ES_DONE:
	    return C_VAL;
	}
    }
}

#endif
