#include <assert.h>

#include "eval.h"
#include "extend.h"			/* XXX */
#include "proc.h"

DEFINE_SPECIAL_FORM("lambda")
{
    obj_t *params = pair_car(F_ARGL);
    obj_t *body = pair_cdr(F_ARGL);
    RETURN(make_procedure(body, params, F_ENV));
}

oldDEFINE_SPECIAL_FORM("quote")
{
    assert(is_null(pair_cdr(ARGLIST)));
    return pair_car(ARGLIST);
}

oldDEFINE_SPECIAL_FORM("define")
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

DEFINE_SPECIAL_FORM("define")
{
    obj_t *var = pair_car(F_ARGL);
    obj_t *rest = pair_cdr(F_ARGL);
    obj_t *value;
    if (is_pair(var)) {
	obj_t *formals = pair_cdr(var);
	var = pair_car(var);
	value = make_procedure(rest, formals, F_ENV);
    } else if (is_null(rest)) {
	value = rest;
    } else {
	assert(is_null(pair_cdr(rest)));
	value = eval(pair_car(rest), F_ENV);
    }
    env_bind(F_ENV, var, BINDING_MUTABLE, value);
    RETURN(make_null());
}

oldDEFINE_SPECIAL_FORM("set!")
{
    obj_t *var = pair_car(ARGLIST);
    binding_t *binding = env_lookup(ENV, var);
    assert(binding_is_mutable(binding));
    obj_t *value = eval(pair_car(pair_cdr(ARGLIST)), ENV);
    binding_set(binding, value);
    return make_null();
}
