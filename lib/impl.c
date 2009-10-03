#include "proc.h"
#include "types.h"

LIBRARY(L"(implementation)");

DEFINE_PROC(L"make-binding")
{
    obj_t *name = pair_car(F_SUBJ);
    int type = fixnum_value(pair_cadr(F_SUBJ));
    obj_t *value = pair_caddr(F_SUBJ);
    RETURN(make_binding(name, type, value));
}

DEFINE_PROC(L"binding-value")
{
    RETURN(binding_value(pair_car(F_SUBJ)));
}

DEFINE_PROC(L"binding-name")
{
    RETURN(binding_name(pair_car(F_SUBJ)));
}

DEFINE_PROC(L"binding-set!")
{
    binding_set_value(pair_car(F_SUBJ), pair_cadr(F_SUBJ));
    RETURN(UNSPECIFIED);
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
