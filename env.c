#include "env.h"

#include <assert.h>
#include <stdio.h>

#define ENV_TRACE 0
#if ENV_TRACE
#include "print.h"
#endif
#include "roots.h"
#include "types.h"

/*
 * An environment is currently implemented as a list of frames.  Each
 * frame is a list of bindings.  A binding is a primitive object with
 * three fields: a name (symbol), a value (object) and a mutability
 * flag.
 */

env_t *make_env(env_t *parent)
{
    return (env_t *) make_pair(NIL, parent);
}

obj_t *join_envs(env_t *an_env, env_t *other_env)
{
    PUSH_ROOT(an_env);
    AUTO_ROOT(env, other_env);
    while (an_env) {
	other_env = make_pair(pair_car(an_env), other_env);
	an_env = pair_cdr(an_env);
    }
    POP_FUNCTION_ROOTS();
    return other_env;
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
#if ENV_TRACE
    printf("lookup(%ls, %O)\n", string_value(symbol_name(var)), env);
#endif
    while (!is_null(env)) {
	obj_t *frame = pair_car(env);
#if ENV_TRACE
	if (pair_cdr(env)) {
	    printf("   FRAME");
	    obj_t *p = frame;
	    while (p) {
		printf(" %O: %O", binding_name(pair_car(p)),
		                  binding_value(pair_car(p)));
		p = pair_cdr(p);
	    }
	    printf("\n");
	} else {
	    printf("   FRAME [builtins]\n");
	}
#endif
	while (!is_null(frame)) {
	    obj_t *binding = pair_car(frame);
	    assert(is_binding(binding));
	    if (binding_name(binding) == var) {
#if ENV_TRACE
		printf("   found\n\n");
#endif
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
