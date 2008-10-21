#include "mem_fixvec.h"

#include <assert.h>
#include <string.h>

#define DEFINE_FIXVEC_TYPE(N)						\
									\
    typedef struct fixvec##N { 						\
        obj_header_t  fv##N##_header; 					\
        obj_t        *fv##N##_ptrs[N]; 					\
    } fixvec##N##_t;							\
									\
    static size_t fv##N##_size_op(const obj_t *obj) 			\
    { 									\
	return sizeof (fixvec##N##_t); 					\
    } 									\
 									\
    static size_t fv##N##_ptr_count_op(const obj_t *obj) 		\
    { 									\
	return N; 							\
    } 									\
 									\
    static void fv##N##_move_op(const obj_t *src, obj_t *dst) 		\
    { 									\
	*(fixvec##N##_t *)dst = *(const fixvec##N##_t*)src;		\
    } 									\
 									\
    static void fv##N##_move_callback_op(const obj_t *src, 		\
			      obj_t *dst, 				\
			      move_callback_t cb) 			\
    { 									\
	int i; 								\
 									\
	const fixvec##N##_t *vsrc = (const fixvec##N##_t *)src; 	\
	fixvec##N##_t *vdst = (fixvec##N##_t *)dst; 			\
	vdst->fv##N##_header = vsrc->fv##N##_header; 			\
	for (i = 0; i < N; i++) 					\
	    vdst->fv##N##_ptrs[i] = cb(vsrc->fv##N##_ptrs[i]); 		\
    } 									\
 									\
    static obj_t *fv##N##_get_ptr_op(const obj_t *obj, size_t index)	\
    { 									\
	fixvec##N##_t *vec = (fixvec##N##_t *)obj; 			\
	if (index < N) 							\
	    return vec->fv##N##_ptrs[index]; 				\
	assert(false); 							\
    } 									\
 									\
    static void fv##N##_set_ptr_op(obj_t *obj, size_t index, obj_t *ptr)\
    { 									\
	fixvec##N##_t *vec = (fixvec##N##_t *)obj; 			\
	if (index < N) 							\
	    vec->fv##N##_ptrs[index] = ptr; 				\
	else 								\
	    assert(false); 						\
    } 									\
 									\
    static mem_ops_t fixvec##N##_ops = { 				\
	L"fixvec" #N, 							\
	NULL, 								\
	NULL, 								\
	NULL,	 							\
	fv##N##_size_op, 						\
	fv##N##_ptr_count_op, 						\
	fv##N##_move_op, 						\
	fv##N##_move_callback_op, 					\
	fv##N##_get_ptr_op, 						\
	fv##N##_set_ptr_op, 						\
	{ } 								\
    };									\
									\
obj_t *fixvec##N##_get_ptr(obj_t *obj, size_t index)			\
{									\
    assert_in_tospace(obj);						\
    assert(index < N);							\
    return ((fixvec##N##_t *)obj)->fv##N##_ptrs[index];			\
}									\
									\
void fixvec##N##_set_ptr(obj_t *obj, size_t index, obj_t *ptr)		\
{									\
    assert_in_tospace(obj);						\
    assert(index < N);							\
    ((fixvec##N##_t *)obj)->fv##N##_ptrs[index] = ptr;			\
}

DEFINE_FIXVEC_TYPE(1)
DEFINE_FIXVEC_TYPE(2)
#if 0
DEFINE_FIXVEC_TYPE(3)
DEFINE_FIXVEC_TYPE(4)
#endif

void mem_fixvec_create_ops(mem_ops_t *ops,
			   wchar_t *name,
			   size_t len,
			   mem_init_op init_op,
			   mem_free_op free_op)
{
    mem_ops_t *super;

    switch (len) {
    case 1:
	super = &fixvec1_ops;
	break;

    case 2:
	super = &fixvec2_ops;
	break;

#if 0
    case 3:
	super = &fixvec3_ops;
	break;

    case 4:
	super = &fixvec4_ops;
	break;
#endif

    default:
	assert(false);
    }

    *ops = *super;
    ops->mo_name = name;
    ops->mo_super = super;
    if (init_op)
	ops->mo_init = init_op;
    if (free_op)
	ops->mo_free = free_op;
}

obj_t *alloc_fixvec1(mem_ops_t *ops, obj_t *ptr0)
{
    assert_in_tospace(ptr0);
    PUSH_ROOT(ptr0);
    obj_t *obj = mem_alloc_obj(ops, sizeof (fixvec1_t));
    fixvec1_t *vec = (fixvec1_t *)obj;
    vec->fv1_ptrs[0] = ptr0;
    verify_heap();
    POP_FUNCTION_ROOTS();
    return obj;
}

obj_t *alloc_fixvec2(mem_ops_t *ops, obj_t *ptr0, obj_t *ptr1)
{
    assert_in_tospace(ptr0);
    assert_in_tospace(ptr1);
    PUSH_ROOT(ptr0);
    PUSH_ROOT(ptr1);
    obj_t *obj = mem_alloc_obj(ops, sizeof (fixvec2_t));
    fixvec2_t *vec = (fixvec2_t *)obj;
    vec->fv2_ptrs[0] = ptr0;
    vec->fv2_ptrs[1] = ptr1;
    verify_heap();
    POP_FUNCTION_ROOTS();
    return obj;
}

#if 0
obj_t *alloc_fixvec3(mem_ops_t *ops, obj_t *ptr0, obj_t *ptr1, obj_t *ptr2)
{
    assert_in_tospace(ptr0);
    assert_in_tospace(ptr1);
    assert_in_tospace(ptr2);
    PUSH_ROOT(ptr0);
    PUSH_ROOT(ptr1);
    PUSH_ROOT(ptr2);
    obj_t *obj = mem_alloc_obj(ops, sizeof (fixvec3_t));
    fixvec3_t *vec = (fixvec3_t *)obj;
    vec->fv3_ptrs[0] = ptr0;
    vec->fv3_ptrs[1] = ptr1;
    vec->fv3_ptrs[2] = ptr2;
    obj = move_obj(obj);
    verify_heap();
    POP_FUNCTION_ROOTS();
    return obj;
}

obj_t *alloc_fixvec4(mem_ops_t *ops,
		     obj_t *ptr0, obj_t *ptr1, obj_t *ptr2, obj_t *ptr3)
{
    assert_in_tospace(ptr0);
    assert_in_tospace(ptr1);
    assert_in_tospace(ptr2);
    assert_in_tospace(ptr3);
    PUSH_ROOT(ptr0);
    PUSH_ROOT(ptr1);
    PUSH_ROOT(ptr2);
    PUSH_ROOT(ptr3);
    obj_t *obj = mem_alloc_obj(ops, sizeof (fixvec4_t));
    fixvec4_t *vec = (fixvec4_t *)obj;
    vec->fv4_ptrs[0] = ptr0;
    vec->fv4_ptrs[1] = ptr1;
    vec->fv4_ptrs[2] = ptr2;
    vec->fv4_ptrs[3] = ptr3;
    obj = move_obj(obj);
    verify_heap();
    POP_FUNCTION_ROOTS();
    return obj;
}
#endif
