#ifndef OBJ_STRING_INCLUDED
#define OBJ_STRING_INCLUDED

#include "obj.h"

#include <stddef.h>

extern obj_t         *make_string(const wchar_t *value);
extern bool           is_string(obj_t *);
extern const wchar_t *string_value(obj_t *);

#endif /* !OBJ_STRING_INCLUDED */
