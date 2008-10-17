#include "obj_fixnum.h"

#include <assert.h>

#include "mem_scalar.h"

typedef struct fixnum_obj {
    obj_header_t fixnum_header;
    int          fixnum_value;
} fixnum_obj_t;

static mem_ops_t fixnum_ops;

static size_t fixnum_size_op(const obj_t *obj)
{
    return sizeof (fixnum_obj_t);
}

obj_t *make_fixnum(int value)
{
    if (!fixnum_ops.mo_super)
	mem_scalar_create_ops(&fixnum_ops, L"fixnum",
			      NULL, NULL, fixnum_size_op);
    obj_t *obj = mem_alloc_obj(&fixnum_ops, sizeof (fixnum_obj_t));
    fixnum_obj_t *fp = (fixnum_obj_t *)obj;
    fp->fixnum_value = value;
    return obj;
}

bool is_fixnum(obj_t *obj)
{
    assert_in_tospace(obj);
    return obj && OBJ_MEM_OPS(obj) == &fixnum_ops;
}

int fixnum_value(obj_t *fixnum)
{
    assert_in_tospace(fixnum);
    assert(is_fixnum(fixnum));
    return ((fixnum_obj_t *)fixnum)->fixnum_value;
}
