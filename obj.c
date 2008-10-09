#include "obj.h"

#include <assert.h>
#include <stdlib.h>
#include <wchar.h>

typedef enum obj_type {
    OB_BOOLEAN,
    OB_FIXNUM,
    OB_CHARACTER,
    OB_STRING,
    OB_SYMBOL,
    OB_PAIR,
    OB_PROCEDURE,
} obj_type_t;

struct object {
    obj_type_t ob_type:8;
    char ob_subtype;
    union {
	bool                                     obu_boolean;
	int                                      obu_fixnum;
	wchar_t					 obu_character;
	struct { wchar_t *obus_value; }          obu_string;
	struct { obj_t *obus_name; }             obu_symbol;
	struct { obj_t *obup_car, *obup_cdr; }   obu_pair;
	struct { obj_t *obup_body, *obup_more; } obu_procedure;
    } ob_u;
};

static obj_t *true_obj, *false_obj;
static obj_t *all_symbols_list;

static obj_t *alloc_obj(obj_type_t type)
{
    /* XXX cheating big time */
    obj_t *op = (obj_t *) malloc(sizeof *op);
    assert(op);
    op->ob_type = type;
    op->ob_subtype = 0;
    return op;
}

/* null methods */

obj_t *make_null(void)
{
    return 0;
}

bool is_null(obj_t *op)
{
    return op == 0;
}

/* boolean methods */

obj_t *make_boolean(bool value)
{
    obj_t **pp = value ? &true_obj : &false_obj;
    if (!*pp) {
	*pp = alloc_obj(OB_BOOLEAN);
	(*pp)->ob_u.obu_boolean = !!value;
    }
    return *pp;
}

bool is_boolean(obj_t *op)
{
    return op && op->ob_type == OB_BOOLEAN;
}

bool boolean_value(obj_t *op)
{
    assert(is_boolean(op));
    return op->ob_u.obu_boolean;
}

/* fixnum methods */

obj_t   *make_fixnum(int value)
{
    obj_t *op = alloc_obj(OB_FIXNUM);
    op->ob_u.obu_fixnum = value;
    return op;
}

bool is_fixnum(obj_t *op)
{
    return op && op->ob_type == OB_FIXNUM;
}

int fixnum_value(obj_t *op)
{
    assert(is_fixnum(op));
    return op->ob_u.obu_fixnum;
}

/* character methods */

obj_t *make_character(wchar_t value)
{
    obj_t *op = alloc_obj(OB_CHARACTER);
    op->ob_u.obu_character = value;
    return op;
}

bool is_character(obj_t *op)
{
    return op && op->ob_type == OB_CHARACTER;
}

wchar_t character_value(obj_t *op)
{
    assert(is_character(op));
    return op->ob_u.obu_character;
}

/* string methods */

obj_t *make_string(wchar_t *value)
{
    obj_t *op = alloc_obj(OB_STRING);
    wchar_t *p = wcsdup(value);		/* XXX cheating again */
    assert(p);
    op->ob_u.obu_string.obus_value = p;
    return op;
}

bool is_string(obj_t *op)
{
    return op && op->ob_type == OB_STRING;
}

wchar_t *string_value(obj_t *op)
{
    assert(is_string(op));
    return op->ob_u.obu_string.obus_value;
}

/* symbol methods */

obj_t *make_symbol(wchar_t *name)
{
    obj_t *p, *sym;
    obj_t *sym_name;

    for (p = all_symbols_list; p; p = pair_cdr(p)) {
	assert(is_pair(p));
	sym = pair_car(p);
	assert(is_symbol(sym));
	sym_name = symbol_name(sym);
	assert(is_string(sym_name));
	if (wcscmp(string_value(sym_name), name) == 0) {
	    return sym;
	}
    }
    /* Not found.  Create one. */
    sym = alloc_obj(OB_SYMBOL);
    sym->ob_u.obu_symbol.obus_name = make_string(name);
    all_symbols_list = make_pair(sym, all_symbols_list);
    return sym;
}

bool is_symbol(obj_t *op)
{
    return op && op->ob_type == OB_SYMBOL;
}

obj_t *symbol_name(obj_t *op)
{
    assert(is_symbol(op));
    return op->ob_u.obu_symbol.obus_name;
}

/* pair methods */

obj_t *make_pair(obj_t *car, obj_t *cdr)
{
    obj_t *op = alloc_obj(OB_PAIR);
    op->ob_u.obu_pair.obup_car = car;
    op->ob_u.obu_pair.obup_cdr = cdr;
    return op;
}

bool is_pair(obj_t *op)
{
    return op && op->ob_type == OB_PAIR;
}

obj_t *pair_car(obj_t *op)
{
    assert(is_pair(op));
    return op->ob_u.obu_pair.obup_car;
}

obj_t *pair_cdr(obj_t *op)
{
    assert(is_pair(op));
    return op->ob_u.obu_pair.obup_cdr;
}

void pair_set_car(obj_t *pair, obj_t *car)
{
    assert(is_pair(pair));
    pair->ob_u.obu_pair.obup_car = car;
}

void pair_set_cdr(obj_t *pair, obj_t *cdr)
{
    assert(is_pair(pair));
    pair->ob_u.obu_pair.obup_cdr = cdr;
}

/* procedure methods */

typedef enum proc_flags {
    PF_COMPILED_C   = 1,
    PF_SPECIAL_FORM = 2,
} proc_type_t;

obj_t *make_procedure(obj_t *body, obj_t *arglist, obj_t *env)
{
    obj_t *op = alloc_obj(OB_PROCEDURE);
    op->ob_subtype = 0;
    op->ob_u.obu_procedure.obup_body = body;
    op->ob_u.obu_procedure.obup_more = make_pair(arglist, env);
    return op;
}

obj_t *make_special_form_procedure(obj_t *body, obj_t *arglist, obj_t *env)
{
    obj_t *op = alloc_obj(OB_PROCEDURE);
    op->ob_subtype = PF_SPECIAL_FORM;
    op->ob_u.obu_procedure.obup_body = body;
    op->ob_u.obu_procedure.obup_more = make_pair(arglist, env);
    return op;
}

obj_t *make_C_procedure(C_procedure_t *code, obj_t *arglist, obj_t *env)
{
    obj_t *op = alloc_obj(OB_PROCEDURE);
    op->ob_subtype = PF_COMPILED_C;
    op->ob_u.obu_procedure.obup_body = (obj_t *) code;
    op->ob_u.obu_procedure.obup_more = make_pair(arglist, env);
    return op;
}

obj_t *make_C_special_form_procedure(C_procedure_t *code,
				     obj_t *arglist,
				     obj_t *env)
{
    obj_t *op = alloc_obj(OB_PROCEDURE);
    op->ob_subtype = PF_COMPILED_C | PF_SPECIAL_FORM;
    op->ob_u.obu_procedure.obup_body = (obj_t *) code;
    op->ob_u.obu_procedure.obup_more = make_pair(arglist, env);
    return op;
}

bool is_procedure(obj_t *op)
{
    return op && op->ob_type == OB_PROCEDURE;
}

bool procedure_is_C(obj_t *op)
{
    assert(is_procedure(op));
    return (op->ob_subtype & PF_COMPILED_C) != 0;
}

bool procedure_is_special_form(obj_t *op)
{
    assert(is_procedure(op));
    return (op->ob_subtype & PF_SPECIAL_FORM) != 0;
}

obj_t *procedure_body(obj_t *op)
{
    assert(is_procedure(op));
    return op->ob_u.obu_procedure.obup_body;
}

obj_t *procedure_args(obj_t *op)
{
    assert(is_procedure(op));
    return pair_car(op->ob_u.obu_procedure.obup_more);
}

obj_t *procedure_env(obj_t *op)
{
    assert(is_procedure(op));
    return pair_cdr(op->ob_u.obu_procedure.obup_more);
}
