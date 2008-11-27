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

static size_t string_size_op(const obj_t *obj)
{
    return len_to_bytes(((string_obj_t *)obj)->string_len);
}

obj_t *make_string(const wchar_t *value)
{
    if (!string_ops.mo_super)
	mem_scalar_create_ops(&string_ops, L"string", string_size_op);
    size_t len = wcslen(value);
    obj_t *obj = mem_alloc_obj(&string_ops, len_to_bytes(len));
    string_obj_t *sp = (string_obj_t *)obj;
    sp->string_len = len;
    wcscpy(sp->string_value, value);
    return obj;
}

bool is_string(obj_t *obj)
{
    assert_in_tospace(obj);
    return obj && OBJ_MEM_OPS(obj) == &string_ops;
}

const wchar_t *string_value(obj_t *string)
{
    assert_in_tospace(string);
    assert(is_string(string));
    return ((string_obj_t *)string)->string_value;
}
