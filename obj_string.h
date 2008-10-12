#ifndef OBJ_STRING_INCLUDED
#define OBJ_STRING_INCLUDED

#include "obj.h"			// XXX
#if !OLD_MEM

#include <stddef.h>

extern obj_t         *make_string(wchar_t *value);
extern bool           is_string(obj_t *);
extern const wchar_t *string_value(obj_t *);

#endif

#endif /* !OBJ_STRING_INCLUDED */
