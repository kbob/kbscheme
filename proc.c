#include "proc.h"

#include <stdlib.h>
#include <wchar.h>

/* XXX Excise these someday. */
DEFINE_PROC("p")
{ return 0; }

DEFINE_SPECIAL_FORM("sf")
{ return 0; }

DEFINE_BLOCK(b)
{ return 0; }

DECLARE_BLOCK(b)

DEFINE_EXTERN_PROC(xp, "xp")
{ return 0; }

DEFINE_STATIC_PROC(sp, "sp")
{ return 0; }

DEFINE_ANONYMOUS_PROC("ap")
{ return 0; }

DEFINE_EXTERN_SPECIAL_FORM(xsf, "xsf")
{ return 0; }

DEFINE_STATIC_SPECIAL_FORM(ssf, "ssf")
{ return 0; }

DEFINE_ANONYMOUS_SPECIAL_FORM("asf")
{ return 0; }

DECLARE_EXTERN_BLOCK(xb)
DECLARE_STATIC_BLOCK(sb)
DECLARE_EXTERN_BLOCK(xb);
DECLARE_STATIC_BLOCK(sb);

DEFINE_EXTERN_BLOCK(xb)
{ return b(sb(FRAME)); }

DEFINE_STATIC_BLOCK(sb)
{ return 0; }

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
    obj_t *code = make_C_procedure(proc, make_null(), env);
    env_bind(env, cstr_to_symbol(name), BINDING_MUTABLE, code);
}

void bind_special_form(C_procedure_t *form, obj_t *library, const char *name)
{
    env_t *env = library_env(library);
    obj_t *code = make_C_special_form_procedure(form, make_null(), env);
    env_bind(env, cstr_to_symbol(name), BINDING_IMMUTABLE, code);
}
