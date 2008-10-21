#include "proc.h"

#include "roots.h"

static proc_descriptor_t *proc_descs;

void bind_proc(C_procedure_t *proc, obj_t *library, const wchar_t *name)
{
    //printf("binding proc %ls\n", name);
    AUTO_ROOT(env, library_env(library));
    AUTO_ROOT(code, make_C_procedure(proc, NIL, env));
    obj_t *sym = make_symbol(name);
    POP_ROOT(code);
    POP_ROOT(env);
    env_bind(env, sym, BINDING_MUTABLE, code);
}

void bind_special_form(C_procedure_t *form,
		       obj_t *library,
		       const wchar_t *name)
{
    //printf("binding special form %ls\n", name);
    AUTO_ROOT(env, library_env(library));
    AUTO_ROOT(code, make_C_special_form_procedure(form, NIL, env));
    obj_t *sym = make_symbol(name);
    env_bind(env, sym, BINDING_IMMUTABLE, code);
    POP_ROOT(code);
    POP_ROOT(env);
}

void register_proc(proc_descriptor_t *desc)
{
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
