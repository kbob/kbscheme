/*
 * Name bindings
 */

#ifndef BIND_INCLUDED
#define BIND_INCLUDED

#include "obj.h"

typedef obj_t env_t;
typedef obj_t binding_t;

typedef enum binding_type {
    BINDING_MUTABLE,
    BINDING_IMMUTABLE,
} binding_type_t;

/* environments */
extern env_t *make_env(env_t *parent);
extern void env_bind(env_t *, obj_t *name, binding_type_t type, obj_t *value);
extern binding_t *env_lookup(env_t *, obj_t *name);

/* bindings */
extern obj_t *binding_name(binding_t *);
extern binding_type_t binding_type(binding_t *);
extern bool binding_is_mutable(binding_t *);
extern obj_t *binding_value(binding_t *);
extern void binding_set(binding_t *, obj_t *value);

#endif /* !BIND_INCLUDED */
