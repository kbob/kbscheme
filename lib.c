#include "lib.h"

#include "types.h"

lib_t *make_library(obj_t *namespec)
{
    return make_pair(make_env(NIL), NULL);
}

env_t *library_env(lib_t *lib)
{
    return pair_car(lib);
}

lib_t *r6rs_base_library(void)
{
    static lib_t *r6rs_base;
    if (!r6rs_base) {
	/* (rnrs base (6)) */
	obj_t *ver = make_pair(make_fixnum(6), NIL);
        obj_t *rnrs = make_symbol(L"rnrs");
	obj_t *base = make_symbol(L"base");
	r6rs_base = make_library(make_pair(rnrs,
					   make_pair(base,
						     make_pair(ver, NIL))));
    }
    return r6rs_base;
}

