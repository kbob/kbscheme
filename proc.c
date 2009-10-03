#include "proc.h"

#include <stdio.h>
#include <stdlib.h>
#include <wchar.h>

#include "roots.h"

ROOT(root_env);

static proc_descriptor_t *proc_descs;
static alias_descriptor_t *alias_descs;

void bind_proc(C_procedure_t *proc, obj_t *library, const wchar_t *name)
{
    AUTO_ROOT(env, library_env(library));
    AUTO_ROOT(code, make_C_procedure(proc, NIL, env));
    obj_t *sym = make_symbol_from_C_str(name);
    POP_FUNCTION_ROOTS();
    env_bind(env, sym, M_MUTABLE, code);
    env_bind(root_env, sym, M_MUTABLE, code);
}

void bind_special_form(C_procedure_t *form,
		       obj_t *library,
		       const wchar_t *name)
{
    AUTO_ROOT(env, library_env(library));
    AUTO_ROOT(code, make_C_special_form_procedure(form, NIL, env));
    obj_t *sym = make_symbol_from_C_str(name);
    POP_FUNCTION_ROOTS();
    env_bind(env, sym, M_IMMUTABLE, code);
    env_bind(root_env, sym, M_IMMUTABLE, code);
}

void bind_transformer(C_procedure_t *form, obj_t *library, const wchar_t *name)
{
    AUTO_ROOT(env, library_env(library));
    AUTO_ROOT(code, make_C_xformer_proc(form, NIL, env));
    obj_t *sym = make_symbol_from_C_str(name);
    POP_FUNCTION_ROOTS();
    env_bind(env, sym, M_IMMUTABLE, code);
    env_bind(root_env, sym, M_IMMUTABLE, code);
}

void register_proc(proc_descriptor_t *desc)
{
#ifndef NDEBUG
    proc_descriptor_t *p = proc_descs;
    while (p) {
	if (!wcscmp(p->pd_name, desc->pd_name)) {
	    fprintf(stderr, "duplicate proc name \"%ls\"\n", p->pd_name);
	    abort();
	}
	p = p->pd_next;
    }
#endif
    desc->pd_next = proc_descs;
    proc_descs = desc;
}

void register_alias(alias_descriptor_t *desc)
{
    desc->ad_next = alias_descs;
    alias_descs = desc;
}

void register_procs(void)
{
    root_env = make_env(NIL);
    while (proc_descs) {
	proc_descriptor_t *desc = proc_descs;
	obj_t *library = find_library_str(desc->pd_libdesc->ld_namespec);
	(*desc->pd_binder)(desc->pd_proc, library, desc->pd_name);
	proc_descs = desc->pd_next;
    }
    AUTO_ROOT(value, NIL);
    AUTO_ROOT(new_env, NIL);
    AUTO_ROOT(old_env, NIL);
    while (alias_descs) {
	alias_descriptor_t *desc = alias_descs;
	const wchar_t *old_namespec = desc->ad_old_libdesc->ld_namespec;
	obj_t *old_library = find_library_str(old_namespec);
	old_env = library_env(old_library);
	obj_t *old_sym = make_symbol_from_C_str(desc->ad_old_name);
	obj_t *binding = env_lookup(old_env, old_sym);
	value = binding_value(binding);
	const wchar_t *new_namespec = desc->ad_new_libdesc->ld_namespec;
	obj_t *new_library = find_library_str(new_namespec);
	new_env = library_env(new_library);
	obj_t *new_symbol = make_symbol_from_C_str(desc->ad_new_name);
	env_bind(new_env, new_symbol, M_IMMUTABLE, value);
	alias_descs = desc->ad_next;
    }
    POP_FUNCTION_ROOTS();
}

/* XXX not sure this belongs here.  Reevaluate. */

obj_t *root_environment(void)
{
    return root_env;
}
