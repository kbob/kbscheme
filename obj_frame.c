#include "obj_frame.h"

#include <assert.h>
#include <string.h>

#include "mem.h"

typedef struct short_frame {
    obj_header_t   sf_header;
    C_procedure_t *sf_continuation;
    obj_t         *sf_parent;
    obj_t         *sf_subject;
    obj_t         *sf_environment;
    obj_t         *sf_value;
} short_frame_t;

typedef struct long_frame {
    obj_header_t   lf_header;
    C_procedure_t *lf_continuation;
    obj_t         *lf_parent;
    obj_t         *lf_subject;
    obj_t         *lf_environment;
    obj_t         *lf_value;
    obj_t         *lf_procedure;
    obj_t         *lf_arg_list;
    obj_t	  *lf_last_arg;
} long_frame_t;

static size_t sf_size_op(const obj_t *obj)
{
    return sizeof (short_frame_t);
}

static size_t sf_ptr_count_op(const obj_t *obj)
{
    return 4;
}

static void sf_move_op(const obj_t *src, obj_t *dst)
{
    memcpy(dst, src, sizeof (short_frame_t));
}

static void sf_move_callback_op(const obj_t *src, obj_t *dst,
				move_callback_t cb)
{
    short_frame_t *fsrc = (short_frame_t *)src;
    short_frame_t *fdst = (short_frame_t *)dst;
    fdst->sf_header       = fsrc->sf_header;
    fdst->sf_continuation = fsrc->sf_continuation;
    fdst->sf_parent       = cb(fsrc->sf_parent);
    fdst->sf_subject      = cb(fsrc->sf_subject);
    fdst->sf_environment  = cb(fsrc->sf_environment);
    fdst->sf_value        = cb(fsrc->sf_value);
}

static obj_t *sf_get_ptr_op(const obj_t *obj, size_t index)
{
    short_frame_t *fp = (short_frame_t *)obj;
    switch (index) {
    case 0:
	return fp->sf_parent;
    case 1:
	return fp->sf_subject;
    case 2:
	return fp->sf_environment;
    case 3:
	return fp->sf_value;
    default:
	assert(false);
    }
}

static void sf_set_ptr_op(obj_t *obj, size_t index, obj_t *ptr)
{
    assert(index == 3);
    ((short_frame_t *)obj)->sf_value = ptr;
}

static mem_ops_t short_frame_ops = {
    L"short frame",
    NULL,
    NULL,
    NULL,
    sf_size_op,
    sf_ptr_count_op,
    sf_move_op,
    sf_move_callback_op,
    sf_get_ptr_op,
    sf_set_ptr_op,
    { }
};

static size_t lf_size_op(const obj_t *obj)
{
    return sizeof (long_frame_t);
}

static size_t lf_ptr_count_op(const obj_t *obj)
{
    return 7;
}

static void lf_move_op(const obj_t *src, obj_t *dst)
{
    memcpy(dst, src, sizeof (long_frame_t));
}

static void lf_move_callback_op(const obj_t *src, obj_t *dst,
				move_callback_t cb)
{
    long_frame_t *fsrc = (long_frame_t *)src;
    long_frame_t *fdst = (long_frame_t *)dst;
    fdst->lf_header       = fsrc->lf_header;
    fdst->lf_continuation = fsrc->lf_continuation;
    fdst->lf_parent       = cb(fsrc->lf_parent);
    fdst->lf_subject      = cb(fsrc->lf_subject);
    fdst->lf_environment  = cb(fsrc->lf_environment);
    fdst->lf_value        = cb(fsrc->lf_value);
    fdst->lf_procedure    = cb(fsrc->lf_procedure);
    fdst->lf_arg_list     = cb(fsrc->lf_arg_list);
    fdst->lf_last_arg     = cb(fsrc->lf_last_arg);
}

static obj_t *lf_get_ptr_op(const obj_t *obj, size_t index)
{
    long_frame_t *fp = (long_frame_t *)obj;
    switch (index) {
    case 0:
	return fp->lf_parent;
    case 1:
	return fp->lf_subject;
    case 2:
	return fp->lf_environment;
    case 3:
	return fp->lf_value;
    case 4:
	return fp->lf_procedure;
    case 5:
	return fp->lf_arg_list;
    case 6:
	return fp->lf_last_arg;
    default:
	assert(false);
    }
}

static void lf_set_ptr_op(obj_t *obj, size_t index, obj_t *ptr)
{
    assert(index == 3);
    ((long_frame_t *)obj)->lf_value = ptr;
}

static mem_ops_t long_frame_ops = {
    L"long frame",
    NULL,
    NULL,
    NULL,
    lf_size_op,
    lf_ptr_count_op,
    lf_move_op,
    lf_move_callback_op,
    lf_get_ptr_op,
    lf_set_ptr_op,
    { }
};

obj_t *make_short_frame(obj_t         *parent,
			C_procedure_t *continuation,
			obj_t         *subject,
			obj_t         *environment)
{
    obj_t *obj = mem_alloc_obj(&short_frame_ops, sizeof (short_frame_t));
    short_frame_t *fp = (short_frame_t *)obj;
    fp->sf_continuation = continuation;
    fp->sf_parent       = parent;
    fp->sf_subject      = subject;
    fp->sf_environment  = environment;
    fp->sf_value        = NIL;
    return obj;
}

obj_t *make_long_frame(obj_t          *parent,
		       C_procedure_t  *continuation,
		       obj_t          *subject,
		       obj_t          *environment,
		       obj_t          *procedure,
		       obj_t          *arg_list,
		       obj_t          *last_arg)
{
    obj_t *obj = mem_alloc_obj(&long_frame_ops, sizeof (long_frame_t));
    long_frame_t *fp = (long_frame_t *)obj;
    fp->lf_continuation = continuation;
    fp->lf_parent       = parent;
    fp->lf_subject      = subject;
    fp->lf_environment  = environment;
    fp->lf_value        = NIL;
    fp->lf_procedure    = procedure;
    fp->lf_arg_list     = arg_list;
    fp->lf_last_arg     = last_arg;
    return obj;
}

bool is_frame(obj_t *obj)
{
    if (is_null(obj))
	return false;
    mem_ops_t *ops = OBJ_MEM_OPS(obj);
    return ops == &short_frame_ops || ops == &long_frame_ops;
}

bool is_long_frame(obj_t *obj)
{
    return obj && OBJ_MEM_OPS(obj) == &long_frame_ops;
}

obj_t *frame_get_parent(obj_t *frame)
{
    assert(is_frame(frame));
    return ((short_frame_t *) frame)->sf_parent;
}

C_procedure_t *frame_get_continuation(obj_t *frame)
{
    assert(is_frame(frame));
    return ((short_frame_t *) frame)->sf_continuation;
}

obj_t *frame_get_value(obj_t *frame)
{
    assert(is_frame(frame));
    return ((short_frame_t *) frame)->sf_value;
}

obj_t *frame_get_subject(obj_t *frame)
{
    assert(is_frame(frame));
    return ((short_frame_t *) frame)->sf_subject;
}

obj_t *frame_get_environment(obj_t *frame)
{
    assert(is_frame(frame));
    return ((short_frame_t *) frame)->sf_environment;
}

extern obj_t *frame_get_procedure(obj_t *frame)
{
    assert(is_long_frame(frame));
    return ((long_frame_t *) frame)->lf_procedure;
}

extern obj_t *frame_get_arg_list(obj_t *frame)
{
    assert(is_long_frame(frame));
    return ((long_frame_t *) frame)->lf_arg_list;
}

extern obj_t *frame_get_last_arg(obj_t *frame)
{
    assert(is_long_frame(frame));
    return ((long_frame_t *) frame)->lf_last_arg;
}
