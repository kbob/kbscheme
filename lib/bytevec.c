#include "proc.h"
#include "test.h"

LIBRARY(L"(rnrs bytevector (6))")

DEFINE_PROC(L"bytevector?")
{
    RETURN(make_boolean(is_bytevector(pair_car(F_SUBJ))));
}

TEST_EVAL(L"(bytevector? #vu8())",	L"#t");
TEST_EVAL(L"(bytevector? #())",		L"#f");
