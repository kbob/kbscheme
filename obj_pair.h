#ifndef OBJ_PAIR_INCLUDED
#define OBJ_PAIR_INCLUDED

#include "obj.h"
#if !OLD_MEM

extern obj_t *make_pair(obj_t *car, obj_t *cdr);
extern bool   is_pair(obj_t *);
extern obj_t *pair_car(obj_t *);
extern obj_t *pair_cdr(obj_t *);
extern void   pair_set_car(obj_t *pair, obj_t *car);
extern void   pair_set_cdr(obj_t *pair, obj_t *cdr);

#endif

#endif /* !OBJ_PAIR_INCLUDED */
