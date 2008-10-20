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
#define INIT_HEAP_WORDS 4096

static word_t initial_heap[INIT_HEAP_WORDS];
static void *from_space, *to_space = initial_heap;
static void *from_space_end, *to_space_end = &initial_heap[INIT_HEAP_WORDS];
static void *next_alloc = initial_heap;
static bool heap_allocation_needed = true;
static size_t heap_size_bytes = INIT_HEAP_WORDS * sizeof (word_t);
static void *scan = initial_heap;	/* XXX need better name */

static bool is_in_tospace(const obj_t *obj)
{
    const void *vobj = obj;
    // return is_null(obj) || (vobj >= to_space && vobj < to_space_end);
    return is_null(obj) || (vobj >= to_space && vobj < next_alloc);
}

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
    scan = to_space;
    word_t *p;
    for (p = (word_t *)to_space; p < (word_t *)to_space_end; p++)
	p->word_word = 0xdeafb0bb;
}

static const mem_ops_t *known_ops[20];
static size_t n_known_ops = 0;

bool is_known_ops(const mem_ops_t *ops)
{
    int i;
    for (i = 0; i < n_known_ops; i++)
	if (ops == known_ops[i])
	    return true;
    return false;
}

void remember_ops(const mem_ops_t *ops)
{
    if (!is_known_ops(ops)) {
	assert(n_known_ops < 19);
	known_ops[n_known_ops++] = ops;
    }
}

#define IS_IN_FROMSPACE(ptr) \
    ((void *)(ptr) >= from_space && (void *)(ptr) < from_space_end)

void verify_object(obj_t *obj, bool scanned)
{
    if (scanned)
	assert(!OBJ_GC_MARK(obj));
    mem_ops_t *ops = OBJ_MEM_OPS(obj);
    assert(is_known_ops(ops));
    size_t i, nptr = ops->mo_ptr_count(obj);
    for (i = 0; i < nptr; i++) {
	obj_t *ptr = ops->mo_get_ptr(obj, i);
	if (scanned || !from_space) {
	    assert(is_in_tospace(ptr));
	    if (ptr)
		assert(is_known_ops(OBJ_MEM_OPS(ptr)));
	}
	else {
	    assert(is_in_tospace(ptr) || IS_IN_FROMSPACE(ptr));
	    if (ptr)
		assert(OBJ_IS_FWD(ptr) || is_known_ops(OBJ_MEM_OPS(ptr)));
	}
    }
}

void verify_heap()
{
    printf("verify %p .. %p\n", to_space, next_alloc);
    void *p = to_space;
    while (p < scan) {
	obj_t *obj = (obj_t *)p;
	mem_ops_t *ops = OBJ_MEM_OPS(obj);
	assert(is_known_ops(ops));
	//printf("   vfy %ls\n", ops->mo_name);
	size_t size = aligned_size(ops->mo_size(obj));
	verify_object(obj, true);
	p += size;
    }
    //printf("   scan\n");
    if (p != scan) {
	printf("verify_heap: to_space=%p\n", to_space);
	printf("                 scan=%p\n", scan);
	printf("                    p=%p\n", p);
	printf("           next_alloc=%p\n", next_alloc);
	printf("         to_space_end=%p\n", to_space_end);
    }
    assert(p == scan);
    while (p < next_alloc) {
	obj_t *obj = (obj_t *)p;
	mem_ops_t *ops = OBJ_MEM_OPS(obj);
	assert(is_known_ops(ops));
	//printf("   vfy %ls\n", ops->mo_name);
	verify_object(obj, false);
	size_t size = aligned_size(ops->mo_size(obj));
	size_t i, nptr = ops->mo_ptr_count(obj);
	for (i = 0; i < nptr; i++)
	    ops->mo_get_ptr(obj, i);
	p += size;
    }
}

obj_t *mem_move_obj(obj_t *obj)
{
    if (is_null(obj) || is_in_tospace(obj))
	return obj;
    if (OBJ_IS_FWD(obj))
	return OBJ_FWD_PTR(obj);
    printf("move: obj=%p ops_word=0x%x\n", obj, OBJ_OPS_WORD(obj));
    assert(is_known_ops(OBJ_MEM_OPS(obj)));
    size_t size = aligned_size(OBJ_MEM_OPS(obj)->mo_size(obj));
    assert(next_alloc + size <= to_space_end);
    obj_t *new_obj = next_alloc;
    next_alloc += size;
    //XXX merge dup code both here and mem_alloc_obj().
    assert(next_alloc <= to_space_end);
    OBJ_MEM_OPS(obj)->mo_move(obj, new_obj);
    OBJ_SET_FWD(obj, new_obj);
    printf("mem_move_obj(%ls %p -> %p) %d ptrs\n",
	   OBJ_MEM_OPS(new_obj)->mo_name, obj, new_obj, OBJ_MEM_OPS(new_obj)->mo_ptr_count(new_obj));
    return new_obj;
}

static void *scan_obj(obj_t *obj)
{
    mem_ops_t *ops = OBJ_MEM_OPS(obj);
    assert(is_known_ops(ops));
    size_t size = aligned_size(ops->mo_size(obj));
    size_t i, n_ptrs = ops->mo_ptr_count(obj);
    for (i = 0; i < n_ptrs; i++)
	ops->mo_set_ptr(obj, i, mem_move_obj(ops->mo_get_ptr(obj, i)));
    return (void *) obj + size;
}

//#include "print.h"			/* XXX */

static void copy_heap()
{
    /* with lock */ {
	verify_heap();
	flip();
	verify_heap();
	root_descriptor_t *desc;
	for (desc = get_thread_roots(); desc; desc = desc->rd_next) {
	    printf("   moving root %ls\n", desc->rd_name);
	    *desc->rd_root = mem_move_obj(*desc->rd_root);
	    verify_heap();
	}
	while (scan < next_alloc) {
	    scan = scan_obj(scan);
	    verify_heap();
	}
	assert(scan == next_alloc);
    }
}

obj_t *mem_alloc_obj(const mem_ops_t *ops, size_t size)
{
    //printf("alloc %ls\n", ops->mo_name);
    verify_heap();
    remember_ops(ops);
    size_t alloc_size = aligned_size(size);
    if (1 || next_alloc > to_space_end - alloc_size) {
	copy_heap();
	assert(next_alloc <= to_space_end - alloc_size && "out of memory");
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

    assert(is_null(obj) || is_in_tospace(obj));
}
