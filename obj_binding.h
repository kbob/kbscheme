#ifndef OBJ_BINDING_INCLUDED
#define OBJ_BINDING_INCLUDED

#include "obj.h"

typedef enum binding_type {
    BINDING_MUTABLE,
    BINDING_IMMUTABLE,
} binding_type_t;

extern obj_t	      *make_binding     (obj_t	       *name,
					 binding_type_t type,
					 obj_t	       *value);
extern bool	      is_binding        (obj_t *);
extern obj_t	     *binding_name      (obj_t *);
extern binding_type_t binding_type      (obj_t *);
extern bool	      binding_is_mutable(obj_t *);
extern obj_t	     *binding_value     (obj_t *);
extern void	      binding_set_type  (obj_t *, binding_type_t type);
extern void	      binding_set_value (obj_t *, obj_t *value);

#endif /* !OBJ_BINDING_INCLUDED */
