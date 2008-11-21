#include "mem_scalar.h"

#include <assert.h>
#include <wchar.h>			/* XXX */
#include <string.h>

size_t scalar_ptr_count_op(const obj_t *obj)
{
    return 0;
}

void scalar_move_op(const obj_t *src, obj_t *dst)
{
    //if (!wcscmp(OBJ_MEM_OPS(src)->mo_name, L"fixnum"))
    //	printf("scalar_move_op(%p -> %p) value=%d\n", src, dst, ((int *) src)[1]);
    memcpy(dst, src, OBJ_MEM_OPS(src)->mo_size(src));
}

void scalar_move_callback_op(const obj_t *src, obj_t *dst, move_callback_t cp)
{
    memcpy(dst, src, OBJ_MEM_OPS(src)->mo_size(src));
}

obj_t *scalar_get_ptr_op(const obj_t *obj, size_t index)
{
    assert(false);
}

void   scalar_set_ptr_op(obj_t *obj, size_t index, obj_t *ptr)
{
    assert(false);
}

static mem_ops_t scalar_ops = {
    NULL,
    NULL,
    NULL,
    scalar_ptr_count_op,
    scalar_move_op,
    scalar_move_callback_op,
    scalar_get_ptr_op,
    scalar_set_ptr_op,
    { }
};

void mem_scalar_create_ops(mem_ops_t *ops, wchar_t *name, mem_size_op size_op)
{
    *ops = scalar_ops;
    ops->mo_super = &scalar_ops;
    ops->mo_name = name;
    ops->mo_size = size_op;
}
