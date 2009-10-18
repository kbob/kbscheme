#include "obj_pair.h"

#include <assert.h>

#include "mem_fixvec.h"

static mem_ops_t pair_ops;

extern obj_t *make_pair(obj_t *car, obj_t *cdr)
{
    assert_in_tospace(car);
    assert_in_tospace(cdr);
    if (!pair_ops.mo_super)
	mem_fixvec_create_ops(&pair_ops, L"pair", 2);
    return alloc_fixvec2(&pair_ops, car, cdr);
}

bool is_pair(obj_t *obj)
{
    assert_in_tospace(obj);
    return !is_null(obj) && OBJ_MEM_OPS(obj) == &pair_ops;
}

obj_t *pair_car(obj_t *pair)
{
    assert_in_tospace(pair);
    assert(is_pair(pair));
    return fixvec2_get_ptr(pair, 0);
}

obj_t *pair_cdr(obj_t *pair)
{
    assert_in_tospace(pair);
    assert(is_pair(pair));
    return fixvec2_get_ptr(pair, 1);
}

void pair_set_car(obj_t *pair, obj_t *car)
{
    assert_in_tospace(pair);
    assert_in_tospace(car);
    assert(is_pair(pair));
    fixvec2_set_ptr(pair, 0, car);
}

void pair_set_cdr(obj_t *pair, obj_t *cdr)
{
    assert_in_tospace(pair);
    assert_in_tospace(cdr);
    assert(is_pair(pair));
    fixvec2_set_ptr(pair, 1, cdr);
}
