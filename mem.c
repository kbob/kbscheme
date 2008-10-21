#include "mem.h"

#include <assert.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <unistd.h>

typedef struct word {
    intptr_t word_word;
} word_t;

#define N_SPACES 100
#define INIT_HEAP_WORDS 65536
//#define INIT_HEAP_WORDS 4096

static void *spaces[N_SPACES];
static size_t current_space;
static word_t initial_heap[INIT_HEAP_WORDS];
/*XXX static*/ void *from_space, *to_space = initial_heap;
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
	//printf("flipping: allocating new heap\n");
#if 0
	to_space = sbrk(heap_size_bytes);
	from_space = sbrk(heap_size_bytes);
	to_space_end = to_space + heap_size_bytes;
	from_space_end = from_space + heap_size_bytes;
	heap_allocation_needed = false;
#else
	from_space = to_space;
	from_space_end = to_space_end;
	spaces[0] = sbrk(N_SPACES * heap_size_bytes);
	int i;
	for (i = 1; i < N_SPACES; i++) {
	    spaces[i] = spaces[i - 1] + heap_size_bytes;
	    assert(!((intptr_t)spaces[0] & 4095));
	    int rc = mprotect(spaces[i], heap_size_bytes, PROT_NONE);
	    if (rc != 0)
		perror("mprotect(PROT_NONE)");
	    
	}
	current_space = 0;
	to_space = spaces[current_space % N_SPACES];
	to_space_end = to_space + heap_size_bytes;
	heap_allocation_needed = false;
#endif
    } else {
	//printf("flipping: reusing old heap\n");
#if 0
	void *tmp = to_space;
	to_space = from_space;
	from_space = tmp;
	tmp = to_space_end;
	to_space_end = from_space_end;
	from_space_end = tmp;
#else
	if (!((intptr_t)from_space & 4095)) {
	    int rc = mprotect(from_space, heap_size_bytes, PROT_NONE);
	    if (rc != 0)
		perror("mprotect(PROT_NONE)");
	}
	current_space++;
	from_space = to_space;
	from_space_end = to_space_end;
	to_space = spaces[current_space % N_SPACES];
	to_space_end = to_space + heap_size_bytes;
	int rc = mprotect(to_space, heap_size_bytes, PROT_READ | PROT_WRITE);
	if (rc != 0)
	    perror("mprotect(READ|WRITE)");
#endif
    }
    next_alloc = to_space;
    scan = to_space;
#if 0
    word_t *p;
    for (p = (word_t *)to_space; p < (word_t *)to_space_end; p++)
	p->word_word = 0xdeafb0bb;
#endif
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
    //printf("verify %p .. %p\n", to_space, next_alloc);
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
#define verify_heap() ((void) 0)	/* XXX */

obj_t *move_obj(obj_t *obj)
{
    if (is_null(obj) || is_in_tospace(obj))
	return obj;
    if (OBJ_IS_FWD(obj))
	return OBJ_FWD_PTR(obj);
    //printf("move: obj=%p ops_word=0x%x\n", obj, OBJ_OPS_WORD(obj));
    assert(is_known_ops(OBJ_MEM_OPS(obj)));
    size_t size = aligned_size(OBJ_MEM_OPS(obj)->mo_size(obj));
    assert(next_alloc + size <= to_space_end);
    obj_t *new_obj = next_alloc;
    next_alloc += size;
    //XXX merge dup code both here and mem_alloc_obj().
    assert(next_alloc <= to_space_end);
    OBJ_MEM_OPS(obj)->mo_move(obj, new_obj);
    OBJ_SET_FWD(obj, new_obj);
    //printf("move_obj(%ls %p -> %p) %d ptrs\n",
    //	   OBJ_MEM_OPS(new_obj)->mo_name, obj, new_obj, OBJ_MEM_OPS(new_obj)->mo_ptr_count(new_obj));
    return new_obj;
}

static void *scan_obj(obj_t *obj)
{
    mem_ops_t *ops = OBJ_MEM_OPS(obj);
    assert(is_known_ops(ops));
    //printf("scan_obj<%ls>(%p)\n", ops->mo_name, obj);
    size_t size = aligned_size(ops->mo_size(obj));
    size_t i, n_ptrs = ops->mo_ptr_count(obj);
    for (i = 0; i < n_ptrs; i++) {
#if 0
	ops->mo_set_ptr(obj, i, move_obj(ops->mo_get_ptr(obj, i)));
#else
	obj_t *p0 = ops->mo_get_ptr(obj, i);
	assert(is_in_tospace(p0) || IS_IN_FROMSPACE(p0));
	obj_t *p1 = move_obj(p0);
	assert(is_in_tospace(p1));
	//printf("   mo_set_ptr<%ls>(%p, %d, %p)\n", ops->mo_name, obj, i, p1);
	ops->mo_set_ptr(obj, i, p1);
#endif
    }
    return (void *) obj + size;
}

#if 0
obj_t *mem_move_obj(obj_t *obj)
{
    obj_t *new_obj = move_obj(obj);
    if (new_obj != obj) {
	void *scanp = new_obj;
	while (scanp < next_alloc)
	    scanp = scan_obj(scanp);
	assert(scanp == next_alloc);
    }
    return new_obj;
}
#endif

//#include "print.h"			/* XXX */

void scan_stack()
{
    intptr_t bottom = 0;
    char *top = getenv("HOME");
    intptr_t *p;
    assert(!((intptr_t)&bottom & 3));
    for (p = &bottom; p < (intptr_t *)top; p++)
	if (IS_IN_FROMSPACE(p))
	    printf("   fromspace %p\n", p);
}

static void copy_heap()
{
    /* with lock */ {
	verify_heap();
	flip();
	verify_heap();
	root_descriptor_t *desc;
	for (desc = get_thread_roots(); desc; desc = desc->rd_next) {
#if 1
	    *desc->rd_root = move_obj(*desc->rd_root);
#else
	    obj_t *tmp = move_obj(*desc->rd_root);
	    printf("   moving root %s::%ls %p -> %p\n",
		   desc->rd_func ? desc->rd_func : "", desc->rd_name, *desc->rd_root, tmp);
	    *desc->rd_root = tmp;
#endif
	    verify_heap();
	}
	while (scan < next_alloc) {
	    scan = scan_obj(scan);
	    verify_heap();
	}
	assert(scan == next_alloc);
	scan_stack();
    }
}

obj_t *mem_alloc_obj(const mem_ops_t *ops, size_t size)
{
    //printf("alloc %ls\n", ops->mo_name);
    verify_heap();
    remember_ops(ops);
    size_t alloc_size = aligned_size(size);
    if (next_alloc > to_space_end - alloc_size) {
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
