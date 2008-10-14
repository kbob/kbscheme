#include "obj_boolean.h"

#include <assert.h>

#include "mem.h"

typedef struct bool_obj {
    obj_header_t bool_header;
} bool_obj_t;

ROOT(true_obj);
ROOT(false_obj);

static size_t bool_size_op(const obj_t *op)
{
    return sizeof (bool_obj_t);
}

static size_t bool_ptr_count_op(const obj_t *op)
{
    return 0;
}

static void bool_copy_op(const obj_t *src, obj_t *dst)
{
    *(bool_obj_t *)dst = *(bool_obj_t *)src;
}

static void bool_copy_callback_op(const obj_t *src,
				  obj_t *dst,
				  copy_callback_t cb)
{
    *(bool_obj_t *)dst = *(bool_obj_t *)src;
}

static obj_t *bool_get_ptr_op(const obj_t *op, size_t index)
{
    return NIL;
}

static void bool_set_ptr_op(obj_t *op, size_t index, obj_t *elem)
{
    assert(false);
}

static mem_ops_t true_ops = {
    L"true",
    NULL,
    NULL,
    NULL,
    bool_size_op,
    bool_ptr_count_op,
    bool_copy_op,
    bool_copy_callback_op,
    bool_get_ptr_op,
    bool_set_ptr_op,
    { }
};

static mem_ops_t false_ops = {
    L"false",
    NULL,
    NULL,
    NULL,
    bool_size_op,
    bool_ptr_count_op,
    bool_copy_op,
    bool_copy_callback_op,
    bool_get_ptr_op,
    bool_set_ptr_op,
    { }
};

obj_t *make_boolean(bool value)
{
    obj_t **pp = value ? &true_obj : &false_obj;
    if (!*pp)
	*pp = mem_alloc_obj(value ? &true_ops : &false_ops,
			    sizeof (bool_obj_t));
    return *pp;
}

bool is_boolean(obj_t *obj)
{
    if (is_null(obj))
	return false;
    mem_ops_t *ops = OBJ_MEM_OPS(obj);
    return ops == &true_ops || ops == &false_ops;
}

bool boolean_value(obj_t *obj)
{
    assert(is_boolean(obj));
    return OBJ_MEM_OPS(obj) == &true_ops;
}
