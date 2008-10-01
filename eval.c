#include "eval.h"

#include <assert.h>

#include "bind.h"

static bool is_self_evaluating(obj_t *expr)
{
    return (is_null(expr) ||
	    is_boolean(expr) ||
	    is_fixnum(expr) ||
	    is_character(expr) ||
	    is_string(expr));
}

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
			 /*obj_t *formals,*/
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
    // obj_t *code = procedure_code(proc);
    // obj_t *formals = procedure_args(proc);
    if (procedure_is_C(proc)) {
        obj_t *code = procedure_code(proc);
	return (*(obj_t * (*)())code)(arglist, env);
    } else
	return eval_apply(proc/*code, formals*/, arglist, env);
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
	if (!procedure_is_syntax(proc))
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
