#include "extend.h"

#include <assert.h>

#include "eval.h"

DEFINE_SYNTAX("lambda")
{
    obj_t *params = pair_car(ARGLIST);
    obj_t *body = pair_cdr(ARGLIST);
    return make_procedure(body, params, ENV);
}

DEFINE_SYNTAX("quote")
{
    assert(is_null(pair_cdr(ARGLIST)));
    return pair_car(ARGLIST);
}

DEFINE_SYNTAX("define")
{
    obj_t *var = pair_car(ARGLIST);
    obj_t *rest = pair_cdr(ARGLIST);
    obj_t *value;
    if (is_pair(var)) {
	obj_t *formals = pair_cdr(var);
	var = pair_car(var);
	value = make_procedure(rest, formals, ENV);
    } else if (is_null(rest)) {
	value = rest;
    } else {
	assert(is_null(pair_cdr(rest)));
	value = eval(pair_car(rest), ENV);
    }
    env_bind(ENV, var, BINDING_MUTABLE, value);
    return make_null();
}

DEFINE_SYNTAX("set!")
{
    obj_t *var = pair_car(ARGLIST);
    binding_t *binding = env_lookup(ENV, var);
    assert(binding_is_mutable(binding));
    obj_t *value = eval(pair_car(pair_cdr(ARGLIST)), ENV);
    binding_set(binding, value);
    return make_null();
}
