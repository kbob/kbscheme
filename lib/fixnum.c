#include <assert.h>

#include "proc.h"
#include "test.h"

LIBRARY(L"(rnrs arithmetic fixnums (6))")

DEFINE_PROC(L"fixnum?")
{
    assert(is_null(pair_cdr(F_SUBJ)));
    RETURN(make_boolean(is_fixnum(pair_car(F_SUBJ))));
}
