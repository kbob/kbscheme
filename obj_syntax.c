#include "obj_syntax.h"

#include <assert.h>

#include "mem_fixvec.h"

static mem_ops_t syntax_ops;

extern obj_t *make_syntax(obj_t *expr, obj_t *wrap)
{
    assert_in_tospace(expr);
    assert_in_tospace(wrap);
    if (!syntax_ops.mo_super)
	mem_fixvec_create_ops(&syntax_ops, L"syntax", 2);
    return alloc_fixvec2(&syntax_ops, expr, wrap);
}

bool is_syntax(obj_t *obj)
{
    assert_in_tospace(obj);
    return !is_null(obj) && OBJ_MEM_OPS(obj) == &syntax_ops;
}

#include "uprintf.h"
obj_t *syntax_expr(obj_t *syntax)
{
    assert_in_tospace(syntax);
    if (!is_syntax(syntax))
	printf_unchecked("syntax_expr: passed %O\n", syntax);
    assert(is_syntax(syntax));
    return fixvec2_get_ptr(syntax, 0);
}

obj_t *syntax_wrap(obj_t *syntax)
{
    assert_in_tospace(syntax);
    assert(is_syntax(syntax));
    return fixvec2_get_ptr(syntax, 1);
}
