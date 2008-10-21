#include "mem_mixvec.h"

#include <assert.h>

#include "mem.h"

#define DEFINE_MIXVEC_TYPE(M, N, MN)					\
									\
    typedef struct mixvec_##MN {					\
	obj_header_t  mv_##MN##_header;					\
	int           mv_##MN##_ints[M];				\
	obj_t        *mv_##MN##_ptrs[N];				\
    } mixvec_##MN##_t;							\
									\
    static size_t mv_##MN##_size_op(const obj_t *obj)			\
    {									\
	return sizeof (mixvec_##MN##_t);				\
    }									\
									\
    static size_t mv_##MN##_ptr_count_op(const obj_t *obj)		\
    {									\
	return N;							\
    }									\
									\
    static void mv_##MN##_move_op(const obj_t *src, obj_t *dst)		\
    {									\
	*(mixvec_##MN##_t *)dst = *(const mixvec_##MN##_t *)src;	\
    }									\
									\
    static void mv_##MN##_move_callback_op(const obj_t *src,		\
					obj_t *dst,			\
					move_callback_t cb)		\
    {									\
	const mixvec_##MN##_t *vsrc = (const mixvec_##MN##_t *)src;	\
	mixvec_##MN##_t *vdst = (mixvec_##MN##_t *)dst;			\
	vdst->mv_##MN##_header = vsrc->mv_##MN##_header;		\
	size_t i;							\
	for (i = 0; i < M; i++)						\
	    vdst->mv_##MN##_ints[i] = vsrc->mv_##MN##_ints[i];		\
	for (i = 0; i < N; i++)						\
	    vdst->mv_##MN##_ptrs[i] = cb(vsrc->mv_##MN##_ptrs[i]);	\
    }									\
									\
    static obj_t *mv_##MN##_get_ptr_op(const obj_t *obj, size_t index)	\
    {									\
	assert(index < N);						\
	return ((mixvec_##MN##_t *)obj)->mv_##MN##_ptrs[index];		\
    }									\
									\
    static void mv_##MN##_set_ptr_op(obj_t *obj, size_t index, obj_t *ptr)\
    {									\
	assert(index < N);						\
	((mixvec_##MN##_t *)obj)->mv_##MN##_ptrs[index] = ptr;		\
    }									\
									\
    static mem_ops_t mixvec_##MN##_ops = {				\
	L"mixvec_" #MN,							\
	NULL,								\
	NULL,								\
	NULL,								\
	mv_##MN##_size_op,						\
	mv_##MN##_ptr_count_op,						\
	mv_##MN##_move_op,						\
	mv_##MN##_move_callback_op,					\
	mv_##MN##_get_ptr_op,						\
	mv_##MN##_set_ptr_op,						\
	{ }								\
    };									\
									\
    obj_t *alloc_mixvec_##MN(mem_ops_t *ops)				\
    {									\
	obj_t *obj = mem_alloc_obj(ops, sizeof (mixvec_##MN##_t));	\
	mixvec_##MN##_t *vec = (mixvec_##MN##_t *)obj;			\
	int i;								\
	for (i = 0; i < M; i++)						\
	    vec->mv_##MN##_ints[i] = 0;					\
	for (i = 0; i < N; i++)						\
	    vec->mv_##MN##_ptrs[i] = NIL;				\
	verify_heap();							\
	return obj;							\
    }									\
									\
    int mixvec_##MN##_get_int(obj_t *obj, size_t index)			\
    {									\
	assert_in_tospace(obj);						\
	assert(index < M);						\
	return ((mixvec_##MN##_t *)obj)->mv_##MN##_ints[index];		\
    }									\
									\
    void mixvec_##MN##_set_int(obj_t *obj, size_t index, int value)	\
    {									\
	assert_in_tospace(obj);						\
	assert(index < M);						\
	((mixvec_##MN##_t *)obj)->mv_##MN##_ints[index] = value;	\
    }									\
									\
    obj_t *mixvec_##MN##_get_ptr(obj_t *obj, size_t index)		\
    {									\
	assert_in_tospace(obj);						\
	assert(index < N);						\
	return ((mixvec_##MN##_t *)obj)->mv_##MN##_ptrs[index];		\
    }									\
									\
    void  mixvec_##MN##_set_ptr(obj_t *obj, size_t index, obj_t *ptr)	\
    {									\
	assert_in_tospace(obj);						\
	assert(index < N);						\
	((mixvec_##MN##_t *)obj)->mv_##MN##_ptrs[index] = ptr;		\
    }

DEFINE_MIXVEC_TYPE(1, 2, 1_2)

void mem_mixvec_create_ops(mem_ops_t  *ops,
			   wchar_t    *name,
			   size_t      nints,
			   size_t      nptrs,
			   mem_init_op init_op,
			   mem_free_op free_op)
{
    mem_ops_t *super;
    if (nints == 1 && nptrs == 2)
	super = &mixvec_1_2_ops;
    else
	assert(false);
    *ops = *super;
    ops->mo_name = name;
    ops->mo_super = super;
    ops->mo_init = init_op;
    ops->mo_free = free_op;
}
