#include "proc.h"

#include <stdlib.h>
#include <wchar.h>

static obj_t *cstr_to_symbol(const char *cstr)
{
    /* XXX should use Unicode strings? */
    size_t len = mbstowcs(NULL, cstr, 0);
    wchar_t *ws = alloca((len + 1) * sizeof *ws);
    mbstowcs(ws, cstr, len + 1);
    return make_symbol(ws);
}

void bind_proc(C_procedure_t *proc, obj_t *library, const char *name)
{
    env_t *env = library_env(library);
    obj_t *code = make_C_procedure(proc, NIL, env);
    env_bind(env, cstr_to_symbol(name), BINDING_MUTABLE, code);
}

void bind_special_form(C_procedure_t *form, obj_t *library, const char *name)
{
    env_t *env = library_env(library);
    obj_t *code = make_C_special_form_procedure(form, NIL, env);
    env_bind(env, cstr_to_symbol(name), BINDING_IMMUTABLE, code);
}
