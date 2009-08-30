#include "obj_eof.h"

#include <assert.h>

#include "mem.h"
#include "mem_scalar.h"
#include "roots.h"

typedef struct eof_obj {
    obj_header_t eof_header;
} eof_obj_t;

static mem_ops_t eof_ops;

static size_t eof_size_op(const obj_t *obj)
{
    return sizeof (eof_obj_t);
}

ROOT_CONSTRUCTOR(eof_obj)
{
    mem_scalar_create_ops(&eof_ops, L"eof", eof_size_op);
    return mem_alloc_obj(&eof_ops, sizeof (eof_obj_t));
}

obj_t *make_eof()
{
    assert_in_tospace(eof_obj);
    return eof_obj;
}

bool is_eof(obj_t *obj)
{
    assert_in_tospace(obj);
    return obj && OBJ_MEM_OPS(obj) == &eof_ops;
}
