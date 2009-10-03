#ifndef OBJ_BINDING_INCLUDED
#define OBJ_BINDING_INCLUDED

#include "obj.h"

typedef enum binding_type {
    BT_CORE,
    BT_LEXICAL,
    BT_MACRO
} binding_type_t;

typedef enum mutability {
    M_IMMUTABLE,
    M_MUTABLE,
} mutability_t;

extern obj_t	      *make_binding         (obj_t         *name,
					     binding_type_t type,
					     mutability_t  is_mutable,
					     obj_t         *value);
extern bool	      is_binding            (obj_t *);
extern obj_t	     *binding_name          (obj_t *);
extern binding_type_t binding_type          (obj_t *);
extern bool	      binding_is_mutable    (obj_t *);
extern obj_t	     *binding_value         (obj_t *);
extern void	      binding_set_mutability(obj_t *, mutability_t _mutable);
extern void	      binding_set_value     (obj_t *, obj_t *value);

#endif /* !OBJ_BINDING_INCLUDED */
