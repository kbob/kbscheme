#include "obj_vector.h"

#include <assert.h>
#include <string.h>

#include "mem.h"
#include "roots.h"

typedef struct vector_obj {
    obj_header_t v_header;
    size_t       v_size;
} vector_obj_t;

static inline size_t len_to_bytes(size_t len)
{
    return sizeof (vector_obj_t) + len * sizeof (obj_t *);
}

static inline obj_t **elem_addr(vector_obj_t *vec, size_t index)
{
    return (obj_t **)&vec[1] + index;
}

static size_t vector_size_op(const obj_t *obj)
{
    const vector_obj_t *vec = (const vector_obj_t *)obj;
    return len_to_bytes(vec->v_size);
}

static size_t vector_ptr_count_op(const obj_t *obj)
{
    const vector_obj_t *vec = (const vector_obj_t *)obj;
    return vec->v_size;
}

static void vector_move_op(const obj_t *src, obj_t *dst)
{
    vector_obj_t *vsrc = (vector_obj_t *) src;
    vector_obj_t *vdst = (vector_obj_t *) dst;
    *vdst = *vsrc;
    size_t i, size = vsrc->v_size;
    for (i = 0; i < size; i++)
	*elem_addr(vdst, i) = *elem_addr(vsrc, i);
}

static void vector_move_callback_op(const obj_t *src,
			  obj_t *dst,
			  move_callback_t cb)
{
    vector_obj_t *vsrc = (vector_obj_t *) src;
    vector_obj_t *vdst = (vector_obj_t *) dst;
    *vdst = *vsrc;
    size_t i, size = vsrc->v_size;
    for (i = 0; i < size; i++)
	*elem_addr(vdst, i) = cb(*elem_addr(vsrc, i));
}

static obj_t *vector_get_ptr_op(const obj_t *obj, size_t index)
{
    vector_obj_t *vec = (vector_obj_t *)obj;
    if (index < vec->v_size)
	return *elem_addr(vec, index);
    assert(false);
}

static void vector_set_ptr_op(obj_t *obj, size_t index, obj_t *ptr)
{
    vector_obj_t *vec = (vector_obj_t *)obj;
    if (index < vec->v_size)
	*elem_addr(vec, index) = ptr;
    else
	assert(false);
}

static mem_ops_t vector_ops = {
    L"vector",
    NULL,
    vector_size_op,
    vector_ptr_count_op,
    vector_move_op,
    vector_move_callback_op,
    vector_get_ptr_op,
    vector_set_ptr_op,
    { }
};

obj_t *make_vector(size_t size, obj_t *fill)
{
    size_t i;

    assert_in_tospace(fill);
    PUSH_ROOT(fill);
    obj_t *obj = mem_alloc_obj(&vector_ops, len_to_bytes(size));
    vector_obj_t *vec = (vector_obj_t *)obj;
    vec->v_size = size;
    for (i = 0; i < size; i++)
	*elem_addr(vec, i) = fill;
    POP_ROOT(fill);
    return obj;
}

bool is_vector(obj_t *obj)
{
    assert_in_tospace(obj);
    return obj && OBJ_MEM_OPS(obj) == &vector_ops;
}

size_t vector_len(obj_t *obj)
{
    assert_in_tospace(obj);
    vector_obj_t *vec = (vector_obj_t *)obj;
    return vec->v_size;
}

obj_t *vector_ref(obj_t *obj, size_t index)
{
    assert_in_tospace(obj);
    vector_obj_t *vec = (vector_obj_t *)obj;
    assert(index < vec->v_size);
    return *elem_addr(vec, index);
}

void vector_set(obj_t *obj, size_t index, obj_t *elem)
{
    assert_in_tospace(obj);
    vector_obj_t *vec = (vector_obj_t *)obj;
    assert(index < vec->v_size);
    *elem_addr(vec, index) = elem;
}
