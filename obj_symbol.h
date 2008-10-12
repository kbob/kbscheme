#ifndef OBJ_SYMBOL_INCLUDED
#define OBJ_SYMBOL_INCLUDED

#include <stddef.h>

#include "obj.h"			// XXX
#if !OLD_MEM

extern obj_t *make_symbol(wchar_t *name);
extern bool   is_symbol(obj_t *);
extern obj_t *symbol_name(obj_t *);

#endif

#endif /* !OBJ_SYMBOL_INCLUDED */
