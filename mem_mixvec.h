#ifndef MEM_MIXVEC_INCLUDED
#define MEM_MIXVEC_INCLUDED

#include "mem.h"

/*
 * A mixvec is an abstract base type for objects that have a fixed
 * number of scalar fields and a fixed number of object pointers.
 * 
 * Example mixvecs: binding, procedure.
 */

extern void mem_mixvec_create_ops(mem_ops_t *,
				  wchar_t *name,
				  size_t word_count,
				  size_t ptr_count,
				  mem_init_op,
				  mem_free_op);


#define DECLARE_MIXVECMN(MN) 						\
    extern obj_t *alloc_mixvec_##MN(mem_ops_t *); 			\
    extern int mixvec_##MN##_get_int(obj_t *obj, size_t index); 	\
    extern void mixvec_##MN##_set_int(obj_t *obj, size_t index, int value); \
    extern obj_t *mixvec_##MN##_get_ptr(obj_t *obj, size_t index); 	\
    extern void  mixvec_##MN##_set_ptr(obj_t *obj, size_t index, obj_t *ptr);

#define DECLARE_MIXVEC(M, N) DECLARE_MIXVECMN(M##_##N)

DECLARE_MIXVEC(1, 2)
/* Declare more as needed. */

#endif /* !MEM_MIXVEC_INCLUDED */
