#ifndef OBJ_INCLUDED
#define OBJ_INCLUDED

#include "bool.h"

typedef struct object obj_t;		/* defined in obj.c */

#define NIL ((obj_t *)0)
extern bool is_null(obj_t *);

#endif /* !OBJ_INCLUDED */
