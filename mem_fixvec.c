#include "mem_fixvec.h"

#include <assert.h>
#include <string.h>

#define DEFINE_FIXVEC_TYPE(N)						\
    typedef struct fixvec##N { 						\
        obj_header_t  fv##N##_header; 					\
        obj_t        *fv##N##_ptrs[N]; 					\
    } fixvec##N##_t;							\
									\
    size_t fv##N##_size_op(const obj_t *obj) 				\
    { 									\
	return sizeof (fixvec##N##_t); 					\
    } 									\
 									\
    size_t fv##N##_ptr_count_op(const obj_t *obj) 			\
    { 									\
	return N; 							\
    } 									\
 									\
    void fv##N##_copy_op(const obj_t *src, obj_t *dst) 			\
    { 									\
	int i; 								\
 									\
	const fixvec##N##_t *vsrc = (fixvec##N##_t *)src; 		\
	fixvec##N##_t *vdst = (fixvec##N##_t *)dst; 			\
	vdst->fv##N##_header = vsrc->fv##N##_header; 			\
	for (i = 0; i < N; i++) 					\
	    vdst->fv##N##_ptrs[i] = vsrc->fv##N##_ptrs[i]; 		\
    } 									\
 									\
    void fv##N##_copy_callback_op(const obj_t *src, 			\
			      obj_t *dst, 				\
			      copy_callback_t cb) 			\
    { 									\
	int i; 								\
 									\
	const fixvec##N##_t *vsrc = (fixvec##N##_t *)src; 		\
	fixvec##N##_t *vdst = (fixvec##N##_t *)dst; 			\
	vdst->fv##N##_header = vsrc->fv##N##_header; 			\
	for (i = 0; i < N; i++) 					\
	    vdst->fv##N##_ptrs[i] = cb(vsrc->fv##N##_ptrs[i]); 		\
    } 									\
 									\
    obj_t *fv##N##_get_ptr_op(const obj_t *obj, size_t index) 		\
    { 									\
	fixvec##N##_t *vec = (fixvec##N##_t *)obj; 			\
	if (index < N) 							\
	    return vec->fv##N##_ptrs[index]; 				\
	assert(false); 							\
    } 									\
 									\
    void fv##N##_set_ptr_op(obj_t *obj, size_t index, obj_t *ptr)	\
    { 									\
	fixvec##N##_t *vec = (fixvec##N##_t *)obj; 			\
	if (index < N) 							\
	    vec->fv##N##_ptrs[index] = ptr; 				\
	else 								\
	    assert(false); 						\
    } 									\
 									\
static mem_ops_t fixvec##N##_ops = { 					\
    L"fixvec" #N, 							\
    NULL, 								\
    NULL, 								\
    NULL,	 							\
    fv##N##_size_op, 							\
    fv##N##_ptr_count_op, 						\
    fv##N##_copy_op, 							\
    fv##N##_copy_callback_op, 						\
    fv##N##_get_ptr_op, 						\
    fv##N##_set_ptr_op, 						\
    { } 								\
};


DEFINE_FIXVEC_TYPE(1)
DEFINE_FIXVEC_TYPE(2)
DEFINE_FIXVEC_TYPE(3)
DEFINE_FIXVEC_TYPE(4)

void mem_fixvec_create_ops(mem_ops_t *ops,
			   wchar_t *name,
			   size_t len,
			   mem_init_op init_op,
			   mem_free_op free_op)
{
    mem_ops_t *super;
    if (len == 1)
	super = &fixvec1_ops;
    else if (len == 2)
	super = &fixvec2_ops;
    else if (len == 3)
	super = &fixvec3_ops;
    else if (len == 4)
	super = &fixvec4_ops;
    else
	assert(false);

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
    obj_t *obj = mem_alloc_obj(ops, sizeof (fixvec1_t));
    fixvec1_t *vec = (fixvec1_t *)obj;
    vec->fv1_ptrs[0] = ptr0;
    return obj;
}

obj_t *alloc_fixvec2(mem_ops_t *ops, obj_t *ptr0, obj_t *ptr1)
{
    obj_t *obj = mem_alloc_obj(ops, sizeof (fixvec2_t));
    fixvec2_t *vec = (fixvec2_t *)obj;
    vec->fv2_ptrs[0] = ptr0;
    vec->fv2_ptrs[1] = ptr1;
    return obj;
}

obj_t *alloc_fixvec3(mem_ops_t *ops, obj_t *ptr0, obj_t *ptr1, obj_t *ptr2)
{
    obj_t *obj = mem_alloc_obj(ops, sizeof (fixvec3_t));
    fixvec3_t *vec = (fixvec3_t *)obj;
    vec->fv3_ptrs[0] = ptr0;
    vec->fv3_ptrs[1] = ptr1;
    vec->fv3_ptrs[2] = ptr2;
    return obj;
}

obj_t *alloc_fixvec4(mem_ops_t *ops,
		     obj_t *ptr0, obj_t *ptr1, obj_t *ptr2, obj_t *ptr3)
{
    obj_t *obj = mem_alloc_obj(ops, sizeof (fixvec4_t));
    fixvec4_t *vec = (fixvec4_t *)obj;
    vec->fv4_ptrs[0] = ptr0;
    vec->fv4_ptrs[1] = ptr1;
    vec->fv4_ptrs[2] = ptr2;
    vec->fv4_ptrs[3] = ptr3;
    return obj;
}
