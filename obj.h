#ifndef OBJ_INCLUDED
#define OBJ_INCLUDED

#include <stddef.h>

#include "bool.h"

typedef struct object obj_t; /* obj_t is abstract and never defined */

#define NIL        ((obj_t *)0)

#define UNSPECIFIED_NAME L"undef"
#define UNSPECIFIED      (make_symbol(UNSPECIFIED_NAME))
#define UNSPECIFIED_REPR UNSPECIFIED_NAME

extern bool is_null(const obj_t *);

extern const wchar_t *object_type_name(const obj_t *);

#endif /* !OBJ_INCLUDED */
