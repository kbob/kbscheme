#include "extend.h"

#include <assert.h>
#include <stdlib.h>
#include <wchar.h>

#include "bind.h"
#include "lib.h"

obj_t *cstr_to_symbol(const char *cstr)
{
    size_t len = mbstowcs(NULL, cstr, 0);
    wchar_t *ws = alloca((len + 1) * sizeof *ws);
    mbstowcs(ws, cstr, len + 1);
    return make_symbol(ws);
}

void register_proc(obj_t *(*proc)(),
		   obj_t *library,
		   const char *name)
{
    env_t *env = library_env(library);
    obj_t *code = make_C_procedure(proc, make_null(), env);
    env_bind(env, cstr_to_symbol(name), BINDING_MUTABLE, code);
}

void register_syntax(obj_t *(*syntax)(),
		     obj_t *library,
		     const char *name)
{
    env_t *env = library_env(library);
    obj_t *code = make_C_syntax_procedure(syntax, make_null(), env);
    env_bind(env, cstr_to_symbol(name), BINDING_IMMUTABLE, code);
}
