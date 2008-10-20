#ifndef MEM_INCLUDED
#define MEM_INCLUDED

#include <stddef.h>
#include <stdint.h>

#include "bool.h"
#include "obj.h"
#include "roots.h"

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

typedef struct mem_ops mem_ops_t;
typedef obj_t *move_callback_t(const obj_t *);

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

extern void set_heap_size_bytes(size_t usable_size_bytes);

extern void assert_in_tospace(const obj_t *);

extern obj_t *mem_alloc_obj(const mem_ops_t *, size_t);

extern obj_t *mem_move_obj(obj_t *);

extern void verify_heap(void);		// XXX

#endif /* !MEM_INCLUDED */
