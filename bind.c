#include "bind.h"

#include <assert.h>
#include <stdio.h>

#include "roots.h"
#include "types.h"

env_t *make_env(env_t *parent)
{
    return (env_t *) make_pair(NIL, parent);
}

void env_bind(env_t *env, obj_t *name, binding_type_t type, obj_t *value)
{
    assert(!is_null(env));
    assert(is_symbol(name));
    PUSH_ROOT(env);
    AUTO_ROOT(binding, make_binding(name, type, value));
    obj_t *frame = pair_car(env);
    frame = make_pair(binding, frame);
    pair_set_car(env, frame);
    POP_FUNCTION_ROOTS();
}

obj_t *env_lookup(env_t *env, obj_t *var)
{
    /*
     * for frame in env:
     *     for binding in frame:
     *         if binding.name == var:
     *             return binding
     * assert False, 'unbound variable'
     */

    assert(is_symbol(var));
    while (!is_null(env)) {
	obj_t *frame = pair_car(env);
	while (!is_null(frame)) {
	    obj_t *binding = pair_car(frame);
	    assert(is_binding(binding));
	    if (binding_name(binding) == var) {
		return binding;
	    }
	    frame = pair_cdr(frame);
	}
	env = pair_cdr(env);
    }
    fprintf(stderr, "unbound variable \"%ls\"\n",
	    string_value(symbol_name(var)));
    assert(false && "unbound variable");
}
