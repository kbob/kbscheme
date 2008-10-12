#ifndef OBJ_FIXNUM_INCLUDED
#define OBJ_FIXNUM_INCLUDED

#include "obj.h"

#if !OLD_MEM

extern obj_t *make_fixnum(int value);
extern bool   is_fixnum(obj_t *);
extern int    fixnum_value(obj_t *);

#endif

#endif /* !OBJ_FIXNUM_INCLUDED */
