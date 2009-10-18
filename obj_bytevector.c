#include "obj_bytevector.h"

#include <assert.h>
#include <string.h>

#include "mem.h"
#include "roots.h"

typedef struct bytevector_obj {
    obj_header_t v_header;
    size_t       v_size;
} bytevector_obj_t;

static inline size_t len_to_bytes(size_t len)
{
    return sizeof (bytevector_obj_t) + len * sizeof (byte_t);
}

static inline byte_t *elem_addr(bytevector_obj_t *vec, size_t index)
{
    return (void *)&vec[1] + index * sizeof (byte_t);
}

static size_t bytevector_size_op(const obj_t *obj)
{
    const bytevector_obj_t *vec = (const bytevector_obj_t *)obj;
    return len_to_bytes(vec->v_size);
}

static size_t bytevector_ptr_count_op(const obj_t *obj)
{
    return 0;
}

static void bytevector_move_op(const obj_t *src, obj_t *dst)
{
    bytevector_obj_t *vsrc = (bytevector_obj_t *) src;
    bytevector_obj_t *vdst = (bytevector_obj_t *) dst;
    *vdst = *vsrc;
    size_t i, size = vsrc->v_size;
    for (i = 0; i < size; i++)
	*elem_addr(vdst, i) = *elem_addr(vsrc, i);
}

static void bytevector_move_callback_op(const obj_t *src,
			  obj_t *dst,
			  move_callback_t cb)
{
    bytevector_obj_t *vsrc = (bytevector_obj_t *) src;
    bytevector_obj_t *vdst = (bytevector_obj_t *) dst;
    *vdst = *vsrc;
    size_t i, size = vsrc->v_size;
    for (i = 0; i < size; i++)
	*elem_addr(vdst, i) = *elem_addr(vsrc, i);
}

static obj_t *bytevector_get_ptr_op(const obj_t *obj, size_t index)
{
    assert(false);
}

static void bytevector_set_ptr_op(obj_t *obj, size_t index, obj_t *ptr)
{
    assert(false);
}

static mem_ops_t bytevector_ops = {
    L"bytevector",
    NULL,
    bytevector_size_op,
    bytevector_ptr_count_op,
    bytevector_move_op,
    bytevector_move_callback_op,
    bytevector_get_ptr_op,
    bytevector_set_ptr_op,
    { }
};

obj_t *make_bytevector(size_t size, byte_t fill)
{
    size_t i;

    obj_t *obj = mem_alloc_obj(&bytevector_ops, len_to_bytes(size));
    bytevector_obj_t *vec = (bytevector_obj_t *)obj;
    vec->v_size = size;
    for (i = 0; i < size; i++)
	*elem_addr(vec, i) = fill;
    return obj;
}

bool is_bytevector(obj_t *obj)
{
    assert_in_tospace(obj);
    return !is_null(obj) && OBJ_MEM_OPS(obj) == &bytevector_ops;
}

size_t bytevector_len(obj_t *obj)
{
    assert_in_tospace(obj);
    bytevector_obj_t *vec = (bytevector_obj_t *)obj;
    return vec->v_size;
}

byte_t bytevector_get(obj_t *obj, size_t index)
{
    assert_in_tospace(obj);
    bytevector_obj_t *vec = (bytevector_obj_t *)obj;
    assert(index < vec->v_size);
    return *elem_addr(vec, index);
}

void bytevector_set(obj_t *obj, size_t index, byte_t elem)
{
    assert_in_tospace(obj);
    bytevector_obj_t *vec = (bytevector_obj_t *)obj;
    assert(index < vec->v_size);
    *elem_addr(vec, index) = elem;
}
