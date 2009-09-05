#include "obj.h"

#include "mem.h"

const wchar_t *object_type_name(const obj_t *obj)
{
    if (is_null(obj))
	return L"null";
    return OBJ_MEM_OPS(obj)->mo_name;
}
