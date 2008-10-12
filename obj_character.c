#include "obj_character.h"

#if !OLD_MEM

#include <assert.h>
#include <stddef.h>

#include "mem.h"
#include "mem_scalar.h"

typedef struct character_obj {
    obj_header_t character_header;
    wchar_t      character_value;
} character_obj_t;

static mem_ops_t character_ops;

static size_t character_size_op(const obj_t *op)
{
    return sizeof (character_obj_t);
}

obj_t *make_character(wchar_t value)
{
    if (!character_ops.mo_super)
	mem_scalar_create_ops(&character_ops, L"character",
			      NULL, NULL, character_size_op);
    obj_t *op = mem_alloc_obj(&character_ops, sizeof (character_obj_t));
    character_obj_t *fp = (character_obj_t *)op;
    fp->character_value = value;
    return op;
}

bool is_character(obj_t *op)
{
    return op && OBJ_MEM_OPS(op) == &character_ops;
}

wchar_t character_value(obj_t *op)
{
    assert(is_character(op));
    return ((character_obj_t *)op)->character_value;
}

#endif
