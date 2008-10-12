#ifndef OBJ_FIXNUM_INCLUDED
#define OBJ_FIXNUM_INCLUDED

#include "obj.h"

extern obj_t *make_fixnum(int value);
extern bool   is_fixnum(obj_t *);
extern int    fixnum_value(obj_t *);

#endif /* !OBJ_FIXNUM_INCLUDED */
