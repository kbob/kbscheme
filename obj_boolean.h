#ifndef OBJ_BOOL_INCLUDED
#define OBJ_BOOL_INCLUDED

#include "obj.h"

extern obj_t *make_boolean(bool value);
extern bool   is_boolean(obj_t *);
extern bool   boolean_value(obj_t *);

#endif /* !OBJ_BOOL_INCLUDED */
