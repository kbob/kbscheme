#include "mem.h"

#include <assert.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

typedef struct word {
    intptr_t word_word;
} word_t;

//#define INIT_HEAP_WORDS 65536
#define INIT_HEAP_WORDS 1000

static word_t initial_heap[INIT_HEAP_WORDS];
static void *from_space, *to_space = initial_heap;
static void *from_space_end, *to_space_end = &initial_heap[INIT_HEAP_WORDS];
static void *next_alloc = initial_heap;
static bool heap_allocation_needed = true;
static size_t heap_size_bytes = INIT_HEAP_WORDS * sizeof (word_t);
static root_descriptor_t *roots;
//static __thread root_descriptor_t *thread_roots;

static size_t aligned_size(size_t size)
{
    const size_t shm1 = __alignof__ (obj_header_t) - 1;
    /* Non-GNU solution:
    assert(!(sizeof (obj_header_t) & (sizeof (obj_header_t) - 1)) &&
	   "obj_header size must be power of two");
    const size_t shm1 = sizeof (obj_header_t) - 1;
    */
    const size_t mask = ~shm1;
    return (size + shm1) & mask;
}

static void flip()
{
    if (heap_allocation_needed) {
	printf("flipping: allocating new heap\n");
	to_space = sbrk(heap_size_bytes);
	from_space = sbrk(heap_size_bytes);
	to_space_end = to_space + heap_size_bytes;
	from_space_end = from_space + heap_size_bytes;
	heap_allocation_needed = false;
    } else {
	printf("flipping: reusing old heap\n");
	void *tmp = to_space;
	to_space = from_space;
	from_space = tmp;
	tmp = to_space_end;
	to_space_end = from_space_end;
	from_space_end = tmp;
    }
    next_alloc = to_space;
}

static obj_t *move_obj(obj_t *obj)
{
    if (is_null(obj))
	return NIL;
    if (OBJ_IS_FWD(obj))
	return OBJ_FWD_PTR(obj);
    size_t size = aligned_size(OBJ_MEM_OPS(obj)->mo_size(obj));
    obj_t *new_obj = next_alloc;
    //XXX merge dup code both here and mem_alloc_obj().
    next_alloc += size;
    assert(next_alloc <= to_space_end);
    OBJ_MEM_OPS(obj)->mo_move(obj, new_obj);
    OBJ_SET_FWD(obj, new_obj);
    return new_obj;
}

static void *scan_obj(obj_t *obj)
{
    mem_ops_t *ops = OBJ_MEM_OPS(obj);
    size_t size = aligned_size(ops->mo_size(obj));
    size_t i, n_ptrs = ops->mo_ptr_count(obj);
    for (i = 0; i < n_ptrs; i++)
	ops->mo_set_ptr(obj, i, move_obj(ops->mo_get_ptr(obj, i)));
    return (void *) obj + size;
}

#include "print.h"			/* XXX */

static void copy_heap()
{
    /* with lock */ {
	flip();
	root_descriptor_t *desc;
	for (desc = roots; desc; desc = desc->rd_next) {
	    printf("flipping root %ls\n", desc->rd_name);
	    *desc->rd_root = move_obj(*desc->rd_root);
	}
	void *scan = to_space;
	while (scan < next_alloc) {
	    scan = scan_obj(scan);
	}
	assert(scan == next_alloc);
    }
}

obj_t *mem_alloc_obj(const mem_ops_t *ops, size_t size)
{
    size_t alloc_size = aligned_size(size);
    if (next_alloc > to_space_end - alloc_size) {
	copy_heap();
    }
    const mem_ops_t **p;
    /* with lock */ {
        p = next_alloc;
        next_alloc += alloc_size;
    }
    *p = ops;
    obj_t *obj = (obj_t *)p;
    if (ops->mo_init)
	ops->mo_init(obj, size);
    return obj;
}

void set_heap_size_bytes(size_t usable_size_bytes)
{
    if (heap_size_bytes != usable_size_bytes) {
	heap_size_bytes = usable_size_bytes;
	heap_allocation_needed = true;
    }	
}

void assert_in_tospace(const obj_t *obj)
{
    const void *vobj = obj;
    assert(is_null(obj) || (vobj >= to_space && vobj < to_space_end));
}

void mem_record_root(root_descriptor_t *desc)
{
    /* with lock */ {
	desc->rd_next = roots;
	roots = desc;
    }
}
