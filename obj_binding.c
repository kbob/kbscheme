#include "obj_binding.h"

#include <assert.h>

#include "mem_mixvec.h"
#include "roots.h"

static mem_ops_t binding_ops;

obj_t *make_binding(obj_t *name, mutability_t mutability, obj_t *value)
{
    assert_in_tospace(name);
    assert(mutability == M_MUTABLE || mutability == M_IMMUTABLE);
    assert_in_tospace(value);
    PUSH_ROOT(name);
    PUSH_ROOT(value);
    if (!binding_ops.mo_super)
	mem_mixvec_create_ops(&binding_ops, L"binding", 1, 2);
    obj_t *binding = alloc_mixvec_1_2(&binding_ops);
    mixvec_1_2_set_int(binding, 0, mutability);
    mixvec_1_2_set_ptr(binding, 0, name);
    mixvec_1_2_set_ptr(binding, 1, value);
    POP_FUNCTION_ROOTS();
    return binding;
}

bool is_binding(obj_t *obj)
{
    return obj && OBJ_MEM_OPS(obj) == &binding_ops;
}

obj_t *binding_name(obj_t *binding)
{
    assert_in_tospace(binding);
    assert(is_binding(binding));
    return mixvec_1_2_get_ptr(binding, 0);
}

mutability_t binding_mutability(obj_t *binding)
{
    assert_in_tospace(binding);
    assert(is_binding(binding));
    return mixvec_1_2_get_int(binding, 0);
}

bool binding_is_mutable(obj_t *binding)
{
    return binding_mutability(binding) == M_MUTABLE;
}

obj_t *binding_value(obj_t *binding)
{
    assert_in_tospace(binding);
    assert(is_binding(binding));
    return mixvec_1_2_get_ptr(binding, 1);
}

void binding_set_mutability(obj_t *binding, mutability_t mutability)
{
    assert_in_tospace(binding);
    assert(is_binding(binding));
    mixvec_1_2_set_int(binding, 0, mutability);
}

void binding_set_value(obj_t *binding, obj_t *value)
{
    assert_in_tospace(binding);
    assert(is_binding(binding));
    assert(binding_is_mutable(binding));
    mixvec_1_2_set_ptr(binding, 1, value);
}
