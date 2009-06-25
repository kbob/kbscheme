#include "proc.h"
#include "test.h"

LIBRARY(L"(rnrs mutable-pairs (6))")

/*
 * 17. Mutables pairs
 *
 * (set-car! pair obj)			# procedure
 * (set-cdr! pair obj)			# procedure
 */

DEFINE_PROC(L"set-car!")
{
    obj_t *pair = pair_car(F_SUBJ);
    obj_t *obj = pair_cadr(F_SUBJ);
    pair_set_car(pair, obj);
    RETURN(UNSPECIFIED);
}

DEFINE_PROC(L"set-cdr!")
{
    obj_t *pair = pair_car(F_SUBJ);
    obj_t *obj = pair_cadr(F_SUBJ);
    pair_set_cdr(pair, obj);
    RETURN(UNSPECIFIED);
}

TEST_EVAL(L"(define a \'(a . b))"
	  L"(set-car! a \'c)"
	  L"a",				L"(c . b)");

TEST_EVAL(L"(define a \'(a . b))"
	  L"(set-cdr! a \'c)"
	  L"a",				L"(a . c)");
