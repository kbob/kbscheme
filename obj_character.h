#ifndef OBJ_CHARACTER_INCLUDED
#define OBJ_CHARACTER_INCLUDED

#include <stddef.h>

#include "obj.h"

#if !OLD_MEM

extern obj_t   *make_character(wchar_t value);
extern bool     is_character(obj_t *);
extern wchar_t  character_value(obj_t *);

#endif

#endif /* !OBJ_CHARACTER_INCLUDED */
