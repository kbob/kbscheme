#include <assert.h>

#include "proc.h"
#include "test.h"

LIBRARY(L"(rnrs arithmetic fixnums (6))")

DEFINE_PROC(L"fixnum?")
{
    assert(is_null(pair_cdr(F_SUBJ)));
    RETURN(make_boolean(is_fixnum(pair_car(F_SUBJ))));
}

DEFINE_PROC(L"integer?")
{
    assert(is_null(pair_cdr(F_SUBJ)));
    RETURN(make_boolean(is_fixnum(pair_car(F_SUBJ))));
}

DEFINE_PROC(L"=")
{
    obj_t *p = F_SUBJ;
    int x = fixnum_value(pair_car(p));
    while (!is_null(p)) {
	if (fixnum_value(pair_car(p)) != x)
	    RETURN(make_boolean(false));
	p = pair_cdr(p);
    }
    RETURN(make_boolean(true));
}

DEFINE_PROC(L"<")
{
    obj_t *p = F_SUBJ;
    int x = fixnum_value(pair_car(p));
    p = pair_cdr(p);
    while (!is_null(p)) {
	int y = fixnum_value(pair_car(p));
	if (!(x < y))
	    RETURN(make_boolean(false));
	x = y;
	p = pair_cdr(p);
    }
    RETURN(make_boolean(true));
}

DEFINE_PROC(L">")
{
    obj_t *p = F_SUBJ;
    int x = fixnum_value(pair_car(p));
    p = pair_cdr(p);
    while (!is_null(p)) {
	int y = fixnum_value(pair_car(p));
	if (!(x > y))
	    RETURN(make_boolean(false));
	x = y;
	p = pair_cdr(p);
    }
    RETURN(make_boolean(true));
}

DEFINE_PROC(L"<=")
{
    obj_t *p = F_SUBJ;
    int x = fixnum_value(pair_car(p));
    p = pair_cdr(p);
    while (!is_null(p)) {
	int y = fixnum_value(pair_car(p));
	if (!(x <= y))
	    RETURN(make_boolean(false));
	x = y;
	p = pair_cdr(p);
    }
    RETURN(make_boolean(true));
}

DEFINE_PROC(L">=")
{
    obj_t *p = F_SUBJ;
    int x = fixnum_value(pair_car(p));
    p = pair_cdr(p);
    while (!is_null(p)) {
	int y = fixnum_value(pair_car(p));
	if (!(x >= y))
	    RETURN(make_boolean(false));
	x = y;
	p = pair_cdr(p);
    }
    RETURN(make_boolean(true));
}

DEFINE_PROC(L"+")
{
    obj_t *p = F_SUBJ;
    int sum = 0;
    while (!is_null(p)) {
	sum += fixnum_value(pair_car(p));
	p = pair_cdr(p);
    }
    RETURN(make_fixnum(sum));
}

TEST_EVAL(L"+",                   L"#<proc-C>");
TEST_EVAL(L"(+)",                 L"0");
TEST_EVAL(L"(+ 3)",               L"3");
TEST_EVAL(L"(+ 3 4)",             L"7");
TEST_EVAL(L"(+ (+ 1 2) (+ 3 4))", L"10");

DEFINE_PROC(L"-")
{
    obj_t *p = F_SUBJ;
    int diff = fixnum_value(pair_car(p));
    p = pair_cdr(p);
    if (is_null(p))
	RETURN(make_fixnum(-diff));
    while (!is_null(p)) {
	diff -= fixnum_value(pair_car(p));
	p = pair_cdr(p);
    }
    RETURN(make_fixnum(diff));
}

DEFINE_PROC(L"*")
{
    obj_t *p = F_SUBJ;
    int prod = 1;
    while (!is_null(p)) {
	prod *= fixnum_value(pair_car(p));
	p = pair_cdr(p);
    }
    RETURN(make_fixnum(prod));
}

DEFINE_PROC(L"div")
{
    int dividend = fixnum_value(pair_car(F_SUBJ));
    int divisor = fixnum_value(pair_cadr(F_SUBJ));
    assert(is_null(pair_cddr(F_SUBJ)));
    RETURN(make_fixnum(dividend / divisor));
}

DEFINE_PROC(L"mod")
{
    int dividend = fixnum_value(pair_car(F_SUBJ));
    int divisor = fixnum_value(pair_cadr(F_SUBJ));
    assert(is_null(pair_cddr(F_SUBJ)));
    RETURN(make_fixnum(dividend % divisor));
}

DEFINE_PROC(L"abs")
{
    assert(is_null(pair_cdr(F_SUBJ)));
    int x = fixnum_value(pair_car(F_SUBJ));
    RETURN(make_fixnum(x < 0 ? -x : x));
}
