#include "mem.h"

#include <assert.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

static void *space_A, *space_B;
static void *to_space, *from_space;
static void *to_space_end, *from_space_end;
static void *next_alloc;

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

extern obj_t *alloc_obj(const mem_ops_t *ops, size_t size)
{
    if (to_space > to_space_end - size)
	assert(false && "mem all gone");
    const mem_ops_t **p;
    /* with lock */ {
        p = next_alloc;
        next_alloc += size;
    }
    *p = ops;
    obj_t *obj = (obj_t *)p;
    ops->mo_init(obj, size);
    return obj;
}
