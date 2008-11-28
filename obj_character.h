#ifndef OBJ_CHARACTER_INCLUDED
#define OBJ_CHARACTER_INCLUDED

#include "obj.h"

extern obj_t   *make_character(wchar_t value);
extern bool     is_character(obj_t *);
extern wchar_t  character_value(obj_t *);

#endif /* !OBJ_CHARACTER_INCLUDED */
