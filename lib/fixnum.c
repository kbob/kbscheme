#include "proc.h"
#include "test.h"

LIBRARY(L"(rnrs arithmetic fixnums (6))")

DEFINE_PROC(L"fixnum?")
{
    RETURN(make_boolean(is_fixnum(pair_car(F_SUBJ))));
}

DEFINE_PROC(L"fxzero?")
{
    RETURN(make_boolean(fixnum_value(pair_car(F_SUBJ)) == 0));
}
