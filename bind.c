#include "bind.h"

#include <assert.h>
#include <stdio.h>

env_t *make_env(env_t *parent)
{
    return (env_t *) make_pair(make_null(), parent);
}

void env_bind(env_t *env, obj_t *name, binding_type_t type, obj_t *value)
{
    assert(!is_null(env));
    assert(is_symbol(name));
    obj_t *frame = pair_car(env);
    obj_t *binding = make_pair(name, make_pair(make_fixnum(type), value));
    frame = make_pair(binding, frame);
    pair_set_car(env, frame);
}

binding_t *env_lookup(env_t *env, obj_t *var)
{
    /*
     * for frame in env:
     *     for binding in frame:
     *         if binding.name == var:
     *             return binding.value
     * assert False, 'unbound variable'
     */

    assert(is_symbol(var));
    while (!is_null(env)) {
	obj_t *frame = pair_car(env);
	while (!is_null(frame)) {
	    obj_t *binding = pair_car(frame);
	    assert(is_symbol(pair_car(binding)));
	    assert(is_symbol(var));
	    if (pair_car(binding) == var)
		return binding;
	    frame = pair_cdr(frame);
	}
	env = pair_cdr(env);
    }
    fprintf(stderr, "unbound variable \"%ls\"",
	    string_value(symbol_name(var)));
    assert(false && "unbound variable");
}

obj_t *binding_name(binding_t *binding)
{
    return pair_car(binding);
}

binding_type_t binding_type(binding_t *binding)
{
    return fixnum_value(pair_car(pair_cdr(binding)));
}

bool binding_is_mutable(binding_t *binding)
{
    return binding_type(binding) == BINDING_MUTABLE;
}

obj_t *binding_value(binding_t *binding)
{
    return pair_cdr(pair_cdr(binding));
}

void binding_set(binding_t *binding, obj_t *value)
{
    assert(binding_is_mutable(binding));
    pair_set_cdr(pair_cdr(binding), value);
}
