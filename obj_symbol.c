#include "obj_symbol.h"

#include <assert.h>
#include <wchar.h>

#include "mem_fixvec.h"
#include "obj_pair.h"
#include "obj_string.h"
#include "roots.h"

static mem_ops_t symbol_ops;

ROOT(all_symbols_list);

static obj_t *find_symbol(obj_t *name)
{
    obj_t *p, *sym;
    obj_t *sym_name;

    assert(is_string(name));
    for (p = all_symbols_list; !is_null(p); p = pair_cdr(p)) {
	assert(is_pair(p));
	sym = pair_car(p);
	assert(is_symbol(sym));
	sym_name = symbol_name(sym);
	assert(is_string(sym_name));
	if (strings_are_equal(sym_name, name)) {
	    return sym;
	}
    }
    return NIL;
}

extern obj_t *make_symbol(obj_t *name)
{
    obj_t *symbol = find_symbol(name);
    if (is_null(symbol)) {
	/* Not found.  Create one. */
	if (!symbol_ops.mo_super)
	    mem_fixvec_create_ops(&symbol_ops, L"symbol", 1);
	symbol = alloc_fixvec1(&symbol_ops, name);
	PUSH_ROOT(symbol);
	/* with lock */ {
	    /* verify symbol still absent. */
	    all_symbols_list = make_pair(symbol, all_symbols_list);
	}
	POP_ROOT(symbol);
    }
    return symbol;
}

extern obj_t *make_symbol_from_C_str(const wchar_t *C_name)
{
    obj_t *name = make_string_from_chars(C_name, wcslen(C_name));
    return make_symbol(name);
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
    return fixvec1_get_ptr(symbol, 0);
}
