#ifndef MEM_INCLUDED
#define MEM_INCLUDED

#include <stddef.h>
#include <stdint.h>

#include "bool.h"
#include "obj.h"

#define ROOT STATIC_ROOT
// ROOT_CONSTRUCTOR(name)
// { init_code; }

#define EXTERN_ROOT(name) GENERAL_ROOT_(extern, name, NULL)
#define STATIC_ROOT(name) GENERAL_ROOT_(static, name, NULL)

#define GENERAL_ROOT_(storage_class, name, init) 			\
    storage_class obj_t *name; 						\
    __attribute__((constructor)) 					\
    static void mem_record_root_ ## name(void) 				\
    { 									\
        mem_record_root(&name, L ## #name, init); 			\
    } 									\

#define OBJ_MARK_MASK 0x1
#define OBJ_OPS_MASK (~(intptr_t) OBJ_MARK_MASK)

#define OBJ_OPS_WORD(obj) (((obj_header_t *)obj)->ob_ops_)
#define OBJ_MEM_OPS(obj) ((mem_ops_t *)(OBJ_OPS_WORD(obj) & OBJ_OPS_MASK))
#define OBJ_GC_MARK(obj) ((bool)(OBJ_OPS_WORD(obj) & OBJ_MARK_MASK))


typedef struct mem_ops mem_ops_t;
typedef obj_t *copy_callback_t(const obj_t *);
typedef obj_t *root_constructor_t(void);

typedef struct obj_header {
    intptr_t ob_ops_;
} obj_header_t;

typedef void   mem_init_op(obj_t *, size_t);
typedef void   mem_free_op(obj_t *);
typedef size_t mem_size_op(const obj_t *);
typedef size_t mem_ptr_count_op(const obj_t *);
typedef void   mem_copy_op(const obj_t *src, obj_t *dst);
typedef void   mem_copy_callback_op(const obj_t *src, obj_t *dst,
				    copy_callback_t);
typedef obj_t *mem_get_ptr_op(const obj_t *, size_t index);
typedef void   mem_set_ptr_op(obj_t *, size_t index, obj_t *);
typedef struct mem_end_marker { } mem_end_marker_t;

struct mem_ops {

    const wchar_t *mo_name;	       /* name of this object class */

    mem_ops_t *mo_super;		/* superclass pointer */

    mem_init_op *mo_init;   /* initialize a newly allocated object. */

    mem_free_op *mo_free;	   /* "finalize" (free) the object. */

    mem_size_op *mo_size;	 /* get the object's size in bytes. */

    /* get the number of pointers in object. */
    mem_ptr_count_op *mo_ptr_count;

    mem_copy_op *mo_copy;   /* copy the object to the new location. */

    /* copy the object to the new location, using the callback for
       each pointer. */
    mem_copy_callback_op *mo_copy_callback;

    mem_get_ptr_op *mo_get_ptr;	/* get the object's i'th heap pointer. */

    mem_set_ptr_op *mo_set_ptr;	/* set the object's i'th heap pointer. */

    mem_end_marker_t mo_end_marker;
};

extern void mem_init_heap(size_t usable_size);

extern obj_t *mem_alloc_obj(const mem_ops_t *, size_t);

extern void mem_record_root(obj_t **root, wchar_t *name, root_constructor_t);

#endif /* !MEM_INCLUDED */
