#ifndef OBJ_SYMBOL_INCLUDED
#define OBJ_SYMBOL_INCLUDED

#include "obj.h"

extern obj_t *make_symbol(const wchar_t *name);
extern bool   is_symbol(obj_t *);
extern obj_t *symbol_name(obj_t *);

#endif /* !OBJ_SYMBOL_INCLUDED */
