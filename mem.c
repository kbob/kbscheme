#include "mem.h"

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include "roots.h"

#define DEBUG_HEAP 0
#if DEBUG_HEAP
#include <stdio.h>
#endif /* DEBUG_HEAP */

#define INITIAL_HEAP_WORDS (1 << 19)
#define INITIAL_HEAP_BYTES (INITIAL_HEAP_WORDS * sizeof (word_t))

/* A word_t is big enough to hold a pointer.  That's all we know. */
typedef struct word {
    intptr_t word_word;
} word_t;

static void *the_heap;
static size_t heap_size_bytes = INITIAL_HEAP_BYTES;
static void *tospace;
static void *tospace_end;
static void *next_alloc;
static void *alloc_end;
static void *next_scan;
static void *fromspace, *fromspace_end;
static bool heap_is_initialized;

static bool is_in_tospace(const obj_t *obj)
{
    const void *vobj = obj;
    return is_null(vobj) || (vobj >= tospace && vobj < next_alloc);
}

static size_t aligned_size(size_t size)
{
#ifdef __GNUC__
    const size_t shm1 = __alignof__ (obj_header_t) - 1;
#else
    assert(!(sizeof (obj_header_t) & (sizeof (obj_header_t) - 1)) &&
	   "obj_header size must be power of two");
    const size_t shm1 = sizeof (obj_header_t) - 1;
#endif
    const size_t mask = ~shm1;
    return (size + shm1) & mask;
}

#if DEBUG_HEAP

    #define IS_IN_FROMSPACE(ptr) \
	((void *)(ptr) >= fromspace && (void *)(ptr) < fromspace_end)

    static bool debug_heap = true;

    #define KNOWN_OPS_SIZE 20		/* increase as needed. */
    static const mem_ops_t *known_ops[KNOWN_OPS_SIZE];
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
	    assert(n_known_ops < KNOWN_OPS_SIZE - 1);
	    known_ops[n_known_ops++] = ops;
	}
    }

    static void verify_object(obj_t *obj, bool scanned)
    {
	if (scanned)
	    assert(!OBJ_IS_FWD(obj));
	mem_ops_t *ops = OBJ_MEM_OPS(obj);
	assert(is_known_ops(ops));
	size_t i, nptr = ops->mo_ptr_count(obj);
	for (i = 0; i < nptr; i++) {
	    obj_t *ptr = ops->mo_get_ptr(obj, i);
	    if (scanned || !fromspace) {
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

    static void verify_heap()
    {
	if (!debug_heap)
	    return;
	void *p = tospace;
	while (p < next_scan) {
	    obj_t *obj = (obj_t *)p;
	    mem_ops_t *ops = OBJ_MEM_OPS(obj);
	    assert(is_known_ops(ops));
	    size_t size = aligned_size(ops->mo_size(obj));
	    verify_object(obj, true);
	    p += size;
	}
	if (p != next_scan) {
	    printf("verify_heap: to_space=%p\n", tospace);
	    printf("            next_scan=%p\n", next_scan);
	    printf("                    p=%p\n", p);
	    printf("           next_alloc=%p\n", next_alloc);
	    printf("         to_space_end=%p\n", tospace_end);
	    printf("            alloc_end=%0\n", alloc_end);
	}
	assert(p == next_scan);
	while (p < next_alloc) {
	    obj_t *obj = (obj_t *)p;
	    mem_ops_t *ops = OBJ_MEM_OPS(obj);
	    assert(is_known_ops(ops));
	    verify_object(obj, false);
	    size_t size = aligned_size(ops->mo_size(obj));
	    size_t i, nptr = ops->mo_ptr_count(obj);
	    for (i = 0; i < nptr; i++)
		ops->mo_get_ptr(obj, i);
	    p += size;
	}
    }

#else

    static bool debug_heap = false;

    #define verify_heap()     ((void)0)
    #define remember_ops(ops) ((void)0)
    #define is_known_ops(ops) (true)

#endif

static void flip()
{
    fromspace = tospace;
    fromspace_end = tospace_end;
    if (tospace == the_heap) {
	tospace = the_heap + heap_size_bytes / 2;
	alloc_end = tospace_end = the_heap + heap_size_bytes;
    } else {
	tospace = the_heap;
	alloc_end = tospace_end = the_heap + heap_size_bytes / 2;
    }
    next_alloc = next_scan = tospace;
    if (debug_heap) {
	word_t *p;
	for (p = (word_t *)tospace; p < (word_t *)tospace_end; p++)
	    p->word_word = 0xdeafb0bb;
    }
}

static obj_t *move_obj(obj_t *obj)
{
    if (is_null(obj) || is_in_tospace(obj))
	return obj;
    if (OBJ_IS_FWD(obj))
	return OBJ_FWD_PTR(obj);
    assert(is_known_ops(OBJ_MEM_OPS(obj)));
    size_t size = aligned_size(OBJ_MEM_OPS(obj)->mo_size(obj));
    assert(next_alloc + size <= alloc_end);
    obj_t *new_obj = next_alloc;
    next_alloc += size;
    assert(next_alloc <= alloc_end);
    OBJ_MEM_OPS(obj)->mo_move(obj, new_obj);
    OBJ_SET_FWD(obj, new_obj);
    return new_obj;
}

static void *scan_obj(obj_t *obj)
{
    mem_ops_t *ops = OBJ_MEM_OPS(obj);
    assert(is_known_ops(ops));
    size_t size = aligned_size(ops->mo_size(obj));
    size_t i, n_ptrs = ops->mo_ptr_count(obj);
    for (i = 0; i < n_ptrs; i++) {
	ops->mo_set_ptr(obj, i, move_obj(ops->mo_get_ptr(obj, i)));
    }
    return (void *)obj + size;
}

static void copy_heap()
{
    /* with lock */ {
	verify_heap();
	flip();
	verify_heap();
	root_descriptor_t *desc;
	for (desc = get_thread_roots(); desc; desc = desc->rd_next) {
	    *desc->rd_root = move_obj(*desc->rd_root);
	    verify_heap();
	}
	while (next_scan < next_alloc) {
	    next_scan = scan_obj(next_scan);
	    verify_heap();
	}
	assert(next_scan == next_alloc);
	if (debug_heap)
	    alloc_end = next_alloc;
	else if (alloc_end - next_alloc < (tospace_end - tospace) / 2)
		 fprintf(stderr, "increase heap size\n");
    }
}

void set_heap_size_bytes(size_t size_bytes)
{
    assert(!heap_is_initialized);
    if (heap_size_bytes != size_bytes) {
	heap_size_bytes = size_bytes;
    }	
}

void init_heap(void)
{
    assert(!heap_is_initialized);
    the_heap = malloc(heap_size_bytes);
    tospace = the_heap;
    alloc_end = tospace_end = the_heap + heap_size_bytes / 2;
    next_alloc = the_heap;
    next_scan = the_heap;
    if (debug_heap)
	alloc_end = next_alloc;
    
    heap_is_initialized = true;
}

obj_t *mem_alloc_obj(const mem_ops_t *ops, size_t size_bytes)
{
    assert(heap_is_initialized);
    verify_heap();
    remember_ops(ops);
    size_t alloc_size = aligned_size(size_bytes);
    if (next_alloc > alloc_end - alloc_size) {
	copy_heap();
	assert(next_alloc <= tospace_end - alloc_size && "out of memory");
    }
    const mem_ops_t **p;
    /* with lock */ {
        p = next_alloc;
        next_alloc += alloc_size;
    }
    *p = ops;
    return (obj_t *)p;
}

#ifndef NDEBUG
void assert_in_tospace(const obj_t *obj)
{
    assert(heap_is_initialized);
    assert(is_null(obj) || is_in_tospace(obj));
}
#endif /* !NDEBUG */
