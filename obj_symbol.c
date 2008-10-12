#include "obj_symbol.h"

#include <assert.h>
#include <wchar.h>

#include "mem_fixvec.h"
#include "obj_pair.h"
#include "obj_string.h"

static mem_ops_t symbol_ops;

ROOT(all_symbols_list);

extern obj_t *make_symbol(wchar_t *name)
{
    obj_t *p, *sym;
    obj_t *sym_name;

    for (p = all_symbols_list; !is_null(p); p = pair_cdr(p)) {
	assert(is_pair(p));
	sym = pair_car(p);
	assert(is_symbol(sym));
	sym_name = symbol_name(sym);
	assert(is_string(sym_name));
	/* XXX use a Scheme function instead of wcscmp(). */
	if (wcscmp(string_value(sym_name), name) == 0)
	    return sym;
    }
    /* Not found.  Create one. */
    if (!symbol_ops.mo_super)
	mem_fixvec_create_ops(&symbol_ops, L"symbol", 1, NULL, NULL);
    sym = alloc_fixvec1(&symbol_ops, make_string(name));
    /* with lock */ {
	/* verify symbol still absent. */
	all_symbols_list = make_pair(sym, all_symbols_list);
    }
    return sym;
}

bool is_symbol(obj_t *obj)
{
    return obj && OBJ_MEM_OPS(obj) == &symbol_ops;
}

obj_t *symbol_name(obj_t *obj)
{
    assert(is_symbol(obj));
    return OBJ_MEM_OPS(obj)->mo_get_ptr(obj, 0);
}
