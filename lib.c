#include "lib.h"

#include "roots.h"
#include "types.h"

ROOT(r6rs_base);

lib_t *make_library(obj_t *namespec)
{
    PUSH_ROOT(namespec);
    AUTO_ROOT(env, make_env(NIL));
    obj_t *lib = make_pair(env, namespec);
    POP_FUNCTION_ROOTS();
    return lib;
}

env_t *library_env(lib_t *lib)
{
    return pair_car(lib);
}

lib_t *r6rs_base_library(void)
{
    if (!r6rs_base) {
	/* (rnrs base (6)) */
	AUTO_ROOT(sym, make_fixnum(6));
	AUTO_ROOT(p, make_pair(sym, NIL));
	p = make_pair(p, NIL);
	sym = make_symbol(L"rnrs");
	p = make_pair(sym, p);
	sym = make_symbol(L"base");
	p = make_pair(sym, p);
	r6rs_base = make_library(p);
	POP_ROOT(p);
	POP_ROOT(sym);
    }
    return r6rs_base;
}
