#ifndef EVAL_INCLUDED
#define EVAL_INCLUDED

#include "obj.h"

extern obj_t *eval(obj_t *expr, obj_t *env);

extern obj_t *eval_expanded(obj_t *expr, obj_t *env);

#endif /* !EVAL_INCLUDED */
