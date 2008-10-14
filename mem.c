#include "mem.h"

#include <assert.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#define INIT_HEAP_WORDS 65536
static struct { long word; } initial_heap[INIT_HEAP_WORDS];
static void *space_A, *space_B;
static void *from_space, *to_space = initial_heap;
static void *from_space_end, *to_space_end = &initial_heap[INIT_HEAP_WORDS];
static void *next_alloc = initial_heap;

void mem_init_heap(size_t usable_size)
{
    to_space = space_A = sbrk(usable_size);
    from_space = space_B = sbrk(usable_size);
    if (space_A == (void *) -1 || space_B == (void *) -1) {
	fprintf(stderr, "heap allocation failed (%u bytes): %s\n",
		2 * usable_size, strerror(errno));
	exit(1);
    }
    to_space_end = to_space + usable_size;
    from_space_end = from_space + usable_size;
    next_alloc = to_space;
}

extern obj_t *mem_alloc_obj(const mem_ops_t *ops, size_t size)
{
    /* XXX use __alignof__ */
    if (next_alloc > to_space_end - size)
	assert(false && "mem all gone");
    const mem_ops_t **p;
    /* with lock */ {
        p = next_alloc;
        next_alloc += size;
    }
    *p = ops;
    obj_t *obj = (obj_t *)p;
    if (ops->mo_init)
	ops->mo_init(obj, size);
    return obj;
}

void mem_record_root(obj_t **root, wchar_t *name, root_constructor_t init)
{
    // printf("root %ls at %p init %p\n", name, root, init);
}

#if 0
So how does gc work?
We run out of memory.
If !fromspace, we allocate a new halfspace.
We start at the roots and copy everything reachable to the new halfspace.

Every read goes through a read barrier:
    if is_in_fromspace(obj)
        obj = *obj;

#endif
