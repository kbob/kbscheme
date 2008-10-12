#include "obj_fixnum.h"

#include <assert.h>

#include "mem_scalar.h"

typedef struct fixnum_obj {
    obj_header_t fixnum_header;
    int          fixnum_value;
} fixnum_obj_t;

static mem_ops_t fixnum_ops;

static size_t fixnum_size_op(const obj_t *op)
{
    return sizeof (fixnum_obj_t);
}

obj_t *make_fixnum(int value)
{
    if (!fixnum_ops.mo_super)
	mem_scalar_create_ops(&fixnum_ops, L"fixnum",
			      NULL, NULL, fixnum_size_op);
    obj_t *op = mem_alloc_obj(&fixnum_ops, sizeof (fixnum_obj_t));
    fixnum_obj_t *fp = (fixnum_obj_t *)op;
    fp->fixnum_value = value;
    return op;
}

bool is_fixnum(obj_t *op)
{
    return op && OBJ_MEM_OPS(op) == &fixnum_ops;
}

int fixnum_value(obj_t *op)
{
    assert(is_fixnum(op));
    return ((fixnum_obj_t *)op)->fixnum_value;
}
