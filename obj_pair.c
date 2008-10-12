#include "obj_pair.h"

#include <assert.h>

#include "mem_fixvec.h"

static mem_ops_t pair_ops;

extern obj_t *make_pair(obj_t *car, obj_t *cdr)
{
    if (!pair_ops.mo_super)
	mem_fixvec_create_ops(&pair_ops, L"pair", 2, NULL, NULL);
    return alloc_fixvec2(&pair_ops, car, cdr);
}

bool is_pair(obj_t *obj)
{
    return obj && OBJ_MEM_OPS(obj) == &pair_ops;
}

obj_t *pair_car(obj_t *pair)
{
    assert(is_pair(pair));
    return OBJ_MEM_OPS(pair)->mo_get_ptr(pair, 0);
}

obj_t *pair_cdr(obj_t *pair)
{
    assert(is_pair(pair));
    return OBJ_MEM_OPS(pair)->mo_get_ptr(pair, 1);
}

void pair_set_car(obj_t *pair, obj_t *car)
{
    assert(is_pair(pair));
    OBJ_MEM_OPS(pair)->mo_set_ptr(pair, 0, car);
}

void pair_set_cdr(obj_t *pair, obj_t *cdr)
{
    assert(is_pair(pair));
    OBJ_MEM_OPS(pair)->mo_set_ptr(pair, 1, cdr);
}
