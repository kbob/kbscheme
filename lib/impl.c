#include "proc.h"
#include "types.h"

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
