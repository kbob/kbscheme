#include "expand.h"

#include <assert.h>

static inline bool is_macro_use(obj_t *form)
{
    if (!is_pair(form))
	return false;
    obj_t *proc = pair_car(form);
    return is_procedure(proc) && procedure_is_transformer(proc);
}

static inline bool is_proc(obj_t *form, C_procedure_t *proc)
{
    return (is_procedure(form) &&
	    procedure_is_C(form) &&
	    (C_procedure_t *)procedure_body(form) == proc);
}

static inline bool is_define_syntax_form(obj_t *form)
{
    return is_proc(form, define_syntax);
}

static inline bool is_define_form(obj_t *form)
{
    return is_proc(form, define);
}

static inline bool is_begin_form(obj_t *form)
{
    return is_proc(form, begin);
}

static inline bool is_let_syntax_form(obj_t *form)
{
    return false;
    //XXX return is_proc(form, let_syntax) || is_proc(form, letrec_syntax);
}

static obj_t *transform(obj_t *form)
{
    assert(is_pair(form));
    obj_t *proc = pair_car(form);
    if (procedure_is_C(proc))
	return ((C_transformer_t *)procedure_body(proc))(form);
}

static void process_defines()
{
}

static obj_t *expand_define_syntax(obj_t *form)
{
    return NIL;
}

static obj_t *expand_define(obj_t *form)
{
    return NIL;
}

static obj_t *expand_begin(obj_t *form)
{
    return NIL;
}

static obj_t *expand_let(obj_t *form)
{
    return NIL;
}

static obj_t *expand_expr(obj_t *form)
{
    return NIL;
}

obj_t *expand(obj_t *form)
{
    if (is_macro_use(form)) {
	return expand(transform(form));
    }
    else if (is_define_syntax_form(form)) {
	return expand_define_syntax(form);
    }
    else if (is_define_form(form)) {
	return expand_define(form);
    }
    else if (is_begin_form(form)) {
	return expand_begin(form);
    }
    else if (is_let_syntax_form(form)) {
	return expand_let(form);
    }
    else {
	process_defines();
	return expand_expr(form);
    }
}
