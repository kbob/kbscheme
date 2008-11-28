#include "proc.h"

#include <stdio.h>
#include <stdlib.h>
#include <wchar.h>

#include "roots.h"

static proc_descriptor_t *proc_descs;

void bind_proc(C_procedure_t *proc, obj_t *library, const wchar_t *name)
{
    AUTO_ROOT(env, library_env(library));
    AUTO_ROOT(code, make_C_procedure(proc, NIL, env));
    obj_t *sym = make_symbol(name);
    POP_FUNCTION_ROOTS();
    env_bind(env, sym, BINDING_MUTABLE, code);
}

void bind_special_form(C_procedure_t *form,
		       obj_t *library,
		       const wchar_t *name)
{
    AUTO_ROOT(env, library_env(library));
    AUTO_ROOT(code, make_C_special_form_procedure(form, NIL, env));
    obj_t *sym = make_symbol(name);
    POP_FUNCTION_ROOTS();
    env_bind(env, sym, BINDING_IMMUTABLE, code);
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

void register_procs(void)
{
    while (proc_descs) {
	proc_descriptor_t *desc = proc_descs;
	lib_t *library = desc->pd_library;
	if (!library)
	    library = r6rs_base_library();
	(*desc->pd_binder)(desc->pd_proc, library, desc->pd_name);
	proc_descs = desc->pd_next;
    }
}
