#include "obj_symbol.h"

#include <assert.h>
#include <wchar.h>

#include "mem_fixvec.h"
#include "obj_pair.h"
#include "obj_string.h"
#include "roots.h"

static mem_ops_t symbol_ops;

ROOT(all_symbols_list);

static obj_t *find_symbol(const wchar_t *name)
{
    obj_t *p, *sym;
    obj_t *sym_name;

    for (p = all_symbols_list; !is_null(p); p = pair_cdr(p)) {
	assert_in_tospace(p);
	assert(is_pair(p));
	sym = pair_car(p);
	assert_in_tospace(sym);
	assert(is_symbol(sym));
	sym_name = symbol_name(sym);
	assert_in_tospace(sym_name);
	assert(is_string(sym_name));
	/* XXX use a Scheme function instead of wcscmp(). */
	if (wcscmp(string_value(sym_name), name) == 0) {
	    return sym;
	}
    }
    return NIL;
}

extern obj_t *make_symbol(const wchar_t *name)
{
    AUTO_ROOT(symbol, find_symbol(name));
    if (is_null(symbol)) {
	/* Not found.  Create one. */
	if (!symbol_ops.mo_super)
	    mem_fixvec_create_ops(&symbol_ops, L"symbol", 1, NULL, NULL);
	symbol = alloc_fixvec1(&symbol_ops, make_string(name));
	/* with lock */ {
	    /* verify symbol still absent. */
	    all_symbols_list = make_pair(symbol, all_symbols_list);
	}
    }
    POP_ROOT(symbol);
    return symbol;
}

bool is_symbol(obj_t *obj)
{
    assert_in_tospace(obj);
    return obj && OBJ_MEM_OPS(obj) == &symbol_ops;
}

obj_t *symbol_name(obj_t *symbol)
{
    assert_in_tospace(symbol);
    assert(is_symbol(symbol));
    return OBJ_MEM_OPS(symbol)->mo_get_ptr(symbol, 0);
}
