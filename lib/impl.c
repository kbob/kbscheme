#include "obj_syntax.h"
#include "proc.h"
#include "types.h"

#include <stdio.h>			/* XXX */

LIBRARY(L"(implementation)");

DEFINE_PROC(L"make-binding")
{
    obj_t *name = pair_car(F_SUBJ);
    int type = fixnum_value(pair_cadr(F_SUBJ));
    int mutability = fixnum_value(pair_caddr(F_SUBJ)); 
    obj_t *value = pair_cadddr(F_SUBJ);
    RETURN(make_binding(name, type, mutability, value));
}

DEFINE_PROC(L"get-binding")
{
    obj_t *sym = pair_car(F_SUBJ);
    obj_t *env = pair_cadr(F_SUBJ);
    RETURN(env_lookup(env, sym));
}

DEFINE_PROC(L"binding-name")
{
    RETURN(binding_name(pair_car(F_SUBJ)));
}

DEFINE_PROC(L"binding-type")
{
    RETURN(make_fixnum(binding_type(pair_car(F_SUBJ))));
}

DEFINE_PROC(L"binding-value")
{
    RETURN(binding_value(pair_car(F_SUBJ)));
}

DEFINE_PROC(L"binding-set!")
{
    binding_set_value(pair_car(F_SUBJ), pair_cadr(F_SUBJ));
    RETURN(UNSPECIFIED);
}

DEFINE_PROC(L"binding-core")
{
    RETURN(make_fixnum(BT_CORE));
}

DEFINE_PROC(L"binding-lexical")
{
    RETURN(make_fixnum(BT_LEXICAL));
}

DEFINE_PROC(L"binding-macro")
{
    RETURN(make_fixnum(BT_MACRO));
}

DEFINE_PROC(L"binding-pattern")
{
    RETURN(make_fixnum(BT_PATTERN));
}

DEFINE_PROC(L"binding-mutable")
{
    RETURN(make_fixnum(M_MUTABLE));
}

DEFINE_PROC(L"binding-immutable")
{
    RETURN(make_fixnum(M_IMMUTABLE));
}

DEFINE_PROC(L"binding-set-mutability!")
{
    binding_set_mutability(pair_car(F_SUBJ), fixnum_value(pair_cadr(F_SUBJ)));
    RETURN(UNSPECIFIED);
}

DEFINE_PROC(L"the-environment")
{
    RETURN(frame_get_environment(frame_get_parent(FRAME)));
}

DEFINE_PROC(L"make-anonymous-symbol")
{
    RETURN(make_anonymous_symbol());
}

DEFINE_PROC(L"make-syntax-object")
{
    RETURN(make_syntax(pair_car(F_SUBJ), pair_cadr(F_SUBJ)));
}

DEFINE_PROC(L"syntax-object?")
{
    RETURN(make_boolean(is_syntax(pair_car(F_SUBJ))));
}

DEFINE_PROC(L"syntax-object-expr")
{
    RETURN(syntax_expr(pair_car(F_SUBJ)));
}

DEFINE_PROC(L"syntax-object-wrap")
{
    RETURN(syntax_wrap(pair_car(F_SUBJ)));
}

DEFINE_PROC(L"special-form?")
{
    obj_t *obj = pair_car(F_SUBJ);
    RETURN(make_boolean(is_procedure(obj) && procedure_is_special_form(obj)));
}

DEFINE_PROC(L"transformer?")
{
    obj_t *obj = pair_car(F_SUBJ);
    RETURN(make_boolean(is_procedure(obj) && procedure_is_xformer(obj)));
}

DEFINE_SPECIAL_FORM(L"plambda")
{
    obj_t *params = pair_car(F_SUBJ);
    obj_t *body = pair_cdr(F_SUBJ);
    RETURN(make_procedure(body, params, F_ENV));
}

DEFINE_PROC(L"newline")
{
    printf("\n");
    RETURN(UNSPECIFIED);
}
