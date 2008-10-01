#ifndef LIB_INCLUDED
#define LIB_INCLUDED

#include "bind.h"
#include "obj.h"

typedef obj_t lib_t;

extern lib_t *make_library(obj_t *namespec);
extern env_t *library_env(lib_t *);

extern lib_t *r6rs_base_library(void);

#endif /* !LIB_INCLUDED */
