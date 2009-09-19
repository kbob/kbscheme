#include "obj_syntax.h"

#include <assert.h>

#include "mem_fixvec.h"

static mem_ops_t syntax_ops;

extern obj_t *make_syntax(obj_t *obj, obj_t *env)
{
    assert_in_tospace(obj);
    assert_in_tospace(env);
    if (!syntax_ops.mo_super)
	mem_fixvec_create_ops(&syntax_ops, L"syntax", 2);
    return alloc_fixvec2(&syntax_ops, obj, env);
}

bool is_syntax(obj_t *obj)
{
    assert_in_tospace(obj);
    return obj && OBJ_MEM_OPS(obj) == &syntax_ops;
}

obj_t *syntax_datum(obj_t *syntax)
{
    assert_in_tospace(syntax);
    assert(is_syntax(syntax));
    return fixvec2_get_ptr(syntax, 0);
}

obj_t *syntax_env(obj_t *syntax)
{
    assert_in_tospace(syntax);
    assert(is_syntax(syntax));
    return fixvec2_get_ptr(syntax, 1);
}
