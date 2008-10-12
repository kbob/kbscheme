#include "obj_bool.h"

#include <assert.h>

#include "mem.h"

#if !OLD_MEM

typedef struct bool_obj {
    mem_ops_t *bool_ops;
} bool_obj_t;

ROOT(true_obj);
ROOT(false_obj);

static void bool_init(const obj_t *op, size_t size)
{}

static size_t bool_size(const obj_t *op)
{
    return sizeof (bool_obj_t);
}

static obj_t *bool_get_ptr(obj_t *op, size_t index)
{
    return NIL;
}

static void bool_set_ptr(obj_t *op, size_t index, obj_t *elem)
{
    assert(false);
}

static bool bool_get_mark(const obj_t *op)
{
    return true;
}

static void bool_set_mark(obj_t *op, bool mark)
{
    assert(mark);
}

static void bool_free(obj_t *op)
{
    assert(false);
}

static mem_ops_t true_ops = {
    L"true",
    NULL,
    bool_init,
    bool_size,
    bool_get_ptr,
    bool_set_ptr,
    bool_get_mark,
    bool_set_mark,
    bool_free
};

static mem_ops_t false_ops = {
    L"false",
    NULL,
    bool_init,
    bool_size,
    bool_get_ptr,
    bool_set_ptr,
    bool_get_mark,
    bool_set_mark,
    bool_free
};

obj_t *make_boolean(bool value)
{
    obj_t **pp = value ? &true_obj : &false_obj;
    if (!*pp)
	*pp = mem_alloc_obj(value ? &true_ops : &false_ops,
			    sizeof (bool_obj_t));
    return *pp;
}

bool is_boolean(obj_t *op)
{
    if (is_null(op))
	return false;
    mem_ops_t *ops = ((bool_obj_t *)op)->bool_ops;
    return ops == &true_ops || ops == &false_ops;
}

bool boolean_value(obj_t *op)
{
    assert(is_boolean(op));
    return ((bool_obj_t *)op)->bool_ops == &true_ops;
}

#endif
