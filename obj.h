#ifndef OBJ_INCLUDED
#define OBJ_INCLUDED

#include "bool.h"

typedef struct object obj_t; /* obj_t is abstract and never defined */

#define NIL ((obj_t *)0)
extern bool is_null(obj_t *);

#endif /* !OBJ_INCLUDED */
