#include "roots.h"

#include <assert.h>
#include <wchar.h>

static root_descriptor_t *static_roots;
static root_descriptor_t *thread_roots;

void record_static_root(root_descriptor_t *desc)
{
    assert(thread_roots == NULL);
    /* with lock */ {
	desc->rd_next = static_roots;
	static_roots = desc;
    }
}

void push_root(root_descriptor_t *desc)
{
    assert(thread_roots < (root_descriptor_t *)0xc0000000 ||
	   desc < thread_roots);
    if (!thread_roots)
	thread_roots = static_roots;
    desc->rd_next = thread_roots;
    thread_roots = desc;
}

void pop_function_roots(const char *func)
{
    while (thread_roots && thread_roots->rd_func == func)
	pop_root(thread_roots->rd_name);
    root_descriptor_t *p;
    for (p = thread_roots; p; p = p->rd_next)
	assert(p->rd_func != func);
}

void pop_root(const wchar_t *name)
{
    assert(!wcscmp(thread_roots->rd_name, name));
    thread_roots = thread_roots->rd_next;
}

root_descriptor_t *get_thread_roots(void)
{
    if (thread_roots)
	return thread_roots;
    else
	return static_roots;
}
