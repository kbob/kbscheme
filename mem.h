#ifndef MEM_INCLUDED
#define MEM_INCLUDED

#include <stddef.h>
#include <stdint.h>

#include "bool.h"
#include "obj.h"

#define ROOT             STATIC_ROOT
#define ROOT_CONSTRUCTOR STATIC_ROOT_CONSTRUCTOR

#define EXTERN_ROOT(name) GENERAL_ROOT_(, name, )
#define STATIC_ROOT(name) GENERAL_ROOT_(static, name, )

#define EXTERN_ROOT_CONSTRUCTOR(name) GENERAL_ROOT_CONSTRUCTOR_(extern, name)
#define STATIC_ROOT_CONSTRUCTOR(name) GENERAL_ROOT_CONSTRUCTOR_(static, name)

#define GENERAL_ROOT_CONSTRUCTOR_(storage_class, name)			\
    DECLARE_ROOT_CONSTRUCTOR_(name);					\
    GENERAL_ROOT_(storage_class, name, name = construct_root_##name())	\
    DECLARE_ROOT_CONSTRUCTOR_(name)
    
#define DECLARE_ROOT_CONSTRUCTOR_(name) \
    __attribute__((constructor)) static obj_t *construct_root_##name(void)

#define GENERAL_ROOT_(storage_class, name, init) 			\
    storage_class obj_t *name = NIL;					\
    __attribute__((constructor)) 					\
    static void mem_record_root_ ## name(void) 				\
    { 									\
	static root_descriptor_t desc = {				\
            L ## #name,							\
	    &name,							\
            NULL							\
        };								\
        mem_record_root(&desc);						\
	init;								\
    }

#define OBJ_MARK_MASK 0x1
#define OBJ_OPS_MASK (~(intptr_t) OBJ_MARK_MASK)

#define OBJ_OPS_WORD(obj) (((obj_header_t *)obj)->ob_ops_)
#define OBJ_GC_MARK(obj) ((bool)(OBJ_OPS_WORD(obj) & OBJ_MARK_MASK))

#define OBJ_MEM_OPS(obj) ((mem_ops_t *)(OBJ_OPS_WORD(obj) & OBJ_OPS_MASK))

#if 0
#define OBJ_IS_FWD(obj) (OBJ_GC_MARK(obj))
#define OBJ_FWD_PTR(obj) ((obj_t *)(OBJ_OPS_WORD(obj) & OBJ_OPS_MASK))
#define OBJ_SET_FWD(obj, fwd) \
    (OBJ_OPS_WORD(obj) = (intptr_t)(fwd) | OBJ_MARK_MASK)

#else
#define OBJ_IS_FWD(obj) (OBJ_GC_MARK(obj))
#define OBJ_FWD_PTR(obj) ((obj_t *)(~OBJ_OPS_WORD(obj)))
#define OBJ_SET_FWD(obj, fwd) \
    (OBJ_OPS_WORD(obj) = ~(intptr_t)(fwd))
#endif

#define ASSERT_IN_TOSPACE(obj) (is_null(obj) || (obj - to_space) < heap_size_bytes)

typedef struct mem_ops mem_ops_t;
typedef obj_t *move_callback_t(const obj_t *);
typedef obj_t *root_constructor_t(void);

typedef struct obj_header {
    intptr_t ob_ops_;
} obj_header_t;

/* initialize a newly allocated object. */
typedef void   mem_init_op(obj_t *, size_t);

/* finalize an object. */
typedef void   mem_free_op(obj_t *);

/* return the object's size in bytes. */
typedef size_t mem_size_op(const obj_t *);

/* return the number of object pointers in the object. */
typedef size_t mem_ptr_count_op(const obj_t *);

/* move the object to the new location. */
typedef void   mem_move_op(const obj_t *src, obj_t *dst);

/* move the object to the new location, using the callback for each pointer. */
typedef void   mem_move_callback_op(const obj_t *src, obj_t *dst,
				    move_callback_t);

/* return the object's i'th heap pointer. */
typedef obj_t *mem_get_ptr_op(const obj_t *, size_t index);

/* set the object's i't heap pointer. */
typedef void   mem_set_ptr_op(obj_t *, size_t index, obj_t *);

typedef struct mem_end_marker { } mem_end_marker_t;

typedef struct root_descriptor root_descriptor_t;

struct root_descriptor {
    const wchar_t        *rd_name;
    obj_t               **rd_root;
    root_descriptor_t    *rd_next;
};

struct mem_ops {
    const wchar_t        *mo_name;	/* object class's name */
    mem_ops_t            *mo_super;	/* superclass pointer */
    mem_init_op          *mo_init;
    mem_free_op          *mo_free;
    mem_size_op          *mo_size;
    mem_ptr_count_op     *mo_ptr_count;
    mem_move_op          *mo_move;
    mem_move_callback_op *mo_move_callback;
    mem_get_ptr_op       *mo_get_ptr;
    mem_set_ptr_op       *mo_set_ptr;
    mem_end_marker_t      mo_end_marker;
};

extern void mem_init_heap(size_t usable_size);

extern obj_t *mem_alloc_obj(const mem_ops_t *, size_t);

extern void mem_record_root(root_descriptor_t *desc);

#endif /* !MEM_INCLUDED */
