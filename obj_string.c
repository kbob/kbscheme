#include "obj_string.h"

#include <assert.h>
#include <wchar.h>

#include "mem_scalar.h"

typedef struct string_obj {
    obj_header_t string_header;
    size_t       string_len;
    wchar_t      string_value[1];	/* NUL-terminated */
} string_obj_t;

static mem_ops_t string_ops;

static size_t len_to_bytes(size_t len)
{
    return sizeof (string_obj_t) + len * sizeof (wchar_t);
}

static size_t string_size_op(const obj_t *op)
{
    return len_to_bytes(((string_obj_t *)op)->string_len);
}

obj_t *make_string(wchar_t *value)
{
    if (!string_ops.mo_super)
	mem_scalar_create_ops(&string_ops, L"string",
			      NULL, NULL, string_size_op);
    size_t len = wcslen(value);
    obj_t *op = mem_alloc_obj(&string_ops, len_to_bytes(len));
    string_obj_t *sp = (string_obj_t *)op;
    wcscpy(sp->string_value, value);
    return op;
}

bool is_string(obj_t *op)
{
    return op && OBJ_MEM_OPS(op) == &string_ops;
}

const wchar_t *string_value(obj_t *op)
{
    assert(is_string(op));
    return ((string_obj_t *)op)->string_value;
}
