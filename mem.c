#include "mem.h"

#include <assert.h>
#include <unistd.h>

#include "roots.h"

#define DEBUG_HEAP 0
#if DEBUG_HEAP
#include <stdio.h>
#endif /* DEBUG_HEAP */

#define INITIAL_HEAP_WORDS 65536
#define INITIAL_HEAP_BYTES (INITIAL_HEAP_WORDS * sizeof (word_t))

/* A word_t is big enough to hold a pointer.  That's all we know. */
typedef struct word {
    intptr_t word_word;
} word_t;

static word_t initial_heap[INITIAL_HEAP_WORDS];

static void *the_heap = initial_heap;
static size_t heap_size_bytes = INITIAL_HEAP_BYTES;
static void *tospace = initial_heap;
static void *tospace_end = initial_heap + INITIAL_HEAP_BYTES / 2;
static void *next_alloc = initial_heap;
static void *next_scan = initial_heap;
static void *fromspace, *fromspace_end;
static bool heap_allocation_needed;

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
    if (heap_allocation_needed) {
	the_heap = sbrk(heap_size_bytes);
	tospace = the_heap;
	tospace_end = tospace + heap_size_bytes / 2;
	heap_allocation_needed = false;
    } else {
	if (tospace == the_heap) {
	    tospace = the_heap + heap_size_bytes / 2;
	    tospace_end = the_heap + heap_size_bytes;
	} else {
	    tospace = the_heap;
	    tospace_end = the_heap + heap_size_bytes / 2;
	}
    }
    next_alloc = next_scan = tospace;
    if (debug_heap) {
	word_t *p;
	for (p = (word_t *)tospace; p < (word_t *)tospace_end; p++)
	    p->word_word = 0xdeafb0bb;
    }
}

obj_t *move_obj(obj_t *obj)
{
    if (is_null(obj) || is_in_tospace(obj))
	return obj;
    if (OBJ_IS_FWD(obj))
	return OBJ_FWD_PTR(obj);
    assert(is_known_ops(OBJ_MEM_OPS(obj)));
    size_t size = aligned_size(OBJ_MEM_OPS(obj)->mo_size(obj));
    assert(next_alloc + size <= tospace_end);
    obj_t *new_obj = next_alloc;
    next_alloc += size;
    assert(next_alloc <= tospace_end);
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
    }
}

extern void set_heap_size_bytes(size_t size_bytes)
{
    if (heap_size_bytes != size_bytes) {
	heap_size_bytes = size_bytes;
	heap_allocation_needed = true;
    }	
}

extern obj_t *mem_alloc_obj(const mem_ops_t *ops, size_t size_bytes)
{
    verify_heap();
    remember_ops(ops);
    size_t alloc_size = aligned_size(size_bytes);
    if (debug_heap || next_alloc > tospace_end - alloc_size) {
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

extern void assert_in_tospace(const obj_t *obj)
{
    assert(is_null(obj) || is_in_tospace(obj));
}
