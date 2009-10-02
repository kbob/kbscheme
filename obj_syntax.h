#ifndef OBJ_SYNTAX_INCLUDED
#define OBJ_SYNTAX_INCLUDED

/* Syntax object (see r6rs-lib section 12.2). */

#include "obj.h"

extern obj_t *make_syntax(obj_t *expr, obj_t *wrap);
extern bool   is_syntax  (obj_t *obj);
extern obj_t *syntax_expr(obj_t *stx);
extern obj_t *syntax_wrap(obj_t *stx);

#endif /* !OBJ_SYNTAX_INCLUDED */
