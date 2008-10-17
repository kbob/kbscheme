#include "obj_procedure.h"

#include <assert.h>
#include <string.h>

#include "mem.h"

obj_t *make_procedure(obj_t *code, obj_t *arglist, obj_t *env);
obj_t *make_C_procedure(C_procedure_t *code, obj_t *arglist, obj_t *env);
obj_t *make_special_form_procedure(obj_t *code, obj_t *arglist, obj_t *env);
obj_t *make_C_special_form_procedure(C_procedure_t *code,
				     obj_t *arglist,
				     obj_t *env);
bool is_procedure(obj_t *);
bool procedure_is_C(obj_t *);
bool procedure_is_special_form(obj_t *);
obj_t *procedure_body(obj_t *);
obj_t *procedure_args(obj_t *);
obj_t *procedure_env(obj_t *);

typedef enum proc_flags {
    PF_COMPILED_C   = 1 << 0,
    PF_SPECIAL_FORM = 1 << 1,
} proc_type_t;

typedef struct proc_obj {
    obj_header_t       proc_header;
    int                proc_flags;
    obj_t             *proc_args;
    obj_t             *proc_env;
    union {
	obj_t           *pu_body;
	C_procedure_t   *pu_code;
    }                  proc_u;
} proc_obj_t;

static size_t proc_size_op(const obj_t *obj)
{
    return sizeof (proc_obj_t);
}

static size_t proc_ptr_count_op(const obj_t *obj)
{
    proc_obj_t *proc = (proc_obj_t *)obj;
    return proc->proc_flags & PF_COMPILED_C ? 2 : 3;
}

static void proc_move_op(const obj_t *src, obj_t *dst)
{
    memcpy(dst, src, sizeof (proc_obj_t));
}

static void proc_move_callback_op(const obj_t *src,
				  obj_t *dst,
				  move_callback_t cb)
{
    const proc_obj_t *psrc = (proc_obj_t *) src;
    proc_obj_t *pdst = (proc_obj_t *) dst;
    pdst->proc_header = psrc->proc_header;
    pdst->proc_flags = psrc->proc_flags;
    pdst->proc_args = cb(psrc->proc_args);
    pdst->proc_env = cb(psrc->proc_env);
    if (psrc->proc_flags & PF_COMPILED_C)
	pdst->proc_u.pu_code = psrc->proc_u.pu_code;
    else
	pdst->proc_u.pu_body = cb(psrc->proc_u.pu_body);
}

static obj_t *proc_get_ptr_op(const obj_t *obj, size_t index)
{
    proc_obj_t *proc = (proc_obj_t *)obj;
    if (index == 0)
	return proc->proc_args;
    if (index == 1)
	return proc->proc_env;
    if (index == 2 && (proc->proc_flags & PF_COMPILED_C))
	return proc->proc_u.pu_body;
    assert(false);
}

void proc_set_ptr_op(obj_t *obj, size_t index, obj_t *ptr)
{
    proc_obj_t *proc = (proc_obj_t *)obj;
    if (index == 0)
	proc->proc_args = ptr;
    else if (index == 1)
	proc->proc_env = ptr;
    else if (index == 2 && (proc->proc_flags & PF_COMPILED_C))
	proc->proc_u.pu_body = ptr;
    else
	assert(false);
}

static mem_ops_t proc_ops = {
    L"procedure",
    NULL,
    NULL,
    NULL,
    proc_size_op,
    proc_ptr_count_op,
    proc_move_op,
    proc_move_callback_op,
    proc_get_ptr_op,
    proc_set_ptr_op,
    { }
};

static obj_t *make_proc(int flags, void *body, obj_t *args, obj_t *env)
{
    obj_t *obj = mem_alloc_obj(&proc_ops, sizeof (proc_obj_t));
    proc_obj_t *proc = (proc_obj_t *)obj;
    proc->proc_flags = flags;
    proc->proc_args = args;
    proc->proc_env = env;
    proc->proc_u.pu_body = body;
    return obj;
}

obj_t *make_procedure(obj_t *body, obj_t *arglist, obj_t *env)
{
    return make_proc(0, body, arglist, env);
}

obj_t *make_special_form_procedure(obj_t *body, obj_t *arglist, obj_t *env)
{
    return make_proc(PF_SPECIAL_FORM, body, arglist, env);
}

obj_t *make_C_procedure(C_procedure_t *code, obj_t *arglist, obj_t *env)
{
    return make_proc(PF_COMPILED_C, code, arglist, env);
}

obj_t *make_C_special_form_procedure(C_procedure_t *code,
				     obj_t *arglist,
				     obj_t *env)
{
    return make_proc(PF_COMPILED_C | PF_SPECIAL_FORM, code, arglist, env);
}

bool is_procedure(obj_t *obj)
{
    return obj && OBJ_MEM_OPS(obj) == &proc_ops;
}

bool procedure_is_C(obj_t *obj)
{
    assert(is_procedure(obj));
    return (((proc_obj_t *)obj)->proc_flags & PF_COMPILED_C) != 0;
}

bool procedure_is_special_form(obj_t *obj)
{
    assert(is_procedure(obj));
    return (((proc_obj_t *)obj)->proc_flags & PF_SPECIAL_FORM) != 0;
}

obj_t *procedure_body(obj_t *obj)
{
    assert(is_procedure(obj));
    return ((proc_obj_t *)obj)->proc_u.pu_body;
}

obj_t *procedure_args(obj_t *obj)
{
    assert(is_procedure(obj));
    return ((proc_obj_t *)obj)->proc_args;
}

obj_t *procedure_env(obj_t *obj)
{
    assert(is_procedure(obj));
    return ((proc_obj_t *)obj)->proc_env;
}
