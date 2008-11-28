#ifndef MEM_INCLUDED
#define MEM_INCLUDED

#include <stdint.h>

#include "obj.h"

/*
 * The first word of each memory object is the "ops word".
 *
 * In an object in the current heap, the ops word is a pointer to a
 * mem_ops_t structure.  You may think of that as a class pointer --
 * the mem_ops_t has the class name and some virtual function
 * pointers for low-level memory operations.
 *
 * While an object is being forwarded to a new heap segment, the old
 * object's ops word is a forwarding pointer to the new object, and
 * the new object's ops word is a normal ops pointer.
 *
 * You can distinguish the ops word from the forwarding pointer
 * because the latter is stored as its one's-complement.  We assume
 * that if the ops word's least significant bit is set, it's a
 * forwarding pointer, and if it's clear, it's an ops pointer.
 */

#define OBJ_MARK_MASK 0x1

#define OBJ_OPS_WORD(obj) (((obj_header_t *)obj)->ob_ops_)

#define OBJ_MEM_OPS(obj) ((mem_ops_t *)(OBJ_OPS_WORD(obj)))

//#define OBJ_IS_FWD(obj) ((intptr_t)(obj) & OBJ_MARK_MASK)
#define OBJ_IS_FWD(obj) (OBJ_OPS_WORD(obj) & OBJ_MARK_MASK)
#define OBJ_FWD_PTR(obj) ((obj_t *)(~OBJ_OPS_WORD(obj)))
#define OBJ_SET_FWD(obj, fwd) (OBJ_OPS_WORD(obj) = ~(intptr_t)(fwd))

typedef struct mem_ops mem_ops_t;
typedef obj_t *move_callback_t(const obj_t *);

typedef struct obj_header {
    intptr_t ob_ops_;
} obj_header_t;

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

extern obj_t *mem_alloc_obj(const mem_ops_t *, size_t size_bytes);

#endif /* !MEM_INCLUDED */
