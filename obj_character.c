#include "obj_character.h"

#include <assert.h>

#include "mem.h"
#include "mem_scalar.h"

typedef struct character_obj {
    obj_header_t character_header;
    wchar_t      character_value;
} character_obj_t;

static mem_ops_t character_ops;

static size_t character_size_op(const obj_t *obj)
{
    return sizeof (character_obj_t);
}

obj_t *make_character(wchar_t value)
{
    if (!character_ops.mo_super)
	mem_scalar_create_ops(&character_ops, L"character", character_size_op);
    obj_t *obj = mem_alloc_obj(&character_ops, sizeof (character_obj_t));
    character_obj_t *cp = (character_obj_t *)obj;
    cp->character_value = value;
    return obj;
}

bool is_character(obj_t *obj)
{
    assert_in_tospace(obj);
    return obj && OBJ_MEM_OPS(obj) == &character_ops;
}

wchar_t character_value(obj_t *character)
{
    assert_in_tospace(character);
    assert(is_character(character));
    return ((character_obj_t *)character)->character_value;
}
