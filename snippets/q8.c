#include <assert.h>
#include <stdio.h>

typedef struct obj_header {
#if 0
    void *p;
#else
    char x[5];				/* fail power of 2 assertion */
#endif
} obj_header_t;

static size_t aligned_size(size_t size)
{
#ifdef _GNU_SOURCE
    const size_t shm1 = __alignof__ (obj_header_t) - 1;
#else
    assert(!(sizeof (obj_header_t) & (sizeof (obj_header_t) - 1)) &&
	   "obj_header size must be power of two");
    const size_t shm1 = sizeof (obj_header_t) - 1;
#endif
    const size_t mask = ~shm1;
    return (size + shm1) & mask;
}

int main()
{
    int size;

    for (size = 0; size < 20; size++)
	printf("align %d => %d\n", size, aligned_size(size));
    return 0;
}
