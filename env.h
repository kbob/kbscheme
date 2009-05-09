#ifndef ENV_INCLUDED
#define ENV_INCLUDED

#include "obj_binding.h"
#include "obj.h"

typedef obj_t env_t;

/* environments */
extern env_t *make_env(env_t *parent);
extern obj_t *join_envs(env_t *an_env, env_t *other_env);
extern void env_bind(env_t *, obj_t *name, binding_type_t type, obj_t *value);
extern obj_t *env_lookup(env_t *, obj_t *name);

#endif /* !ENV_INCLUDED */
