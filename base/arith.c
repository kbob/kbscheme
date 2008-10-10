#include <assert.h>

#include "proc.h"

DEFINE_PROC("number?")
{
    assert(is_null(pair_cdr(F_SUBJ)));
    RETURN(make_boolean(is_fixnum(pair_car(F_SUBJ))));
}

DEFINE_PROC("integer?")
{
    assert(is_null(pair_cdr(F_SUBJ)));
    RETURN(make_boolean(is_fixnum(pair_car(F_SUBJ))));
}

DEFINE_PROC("=")
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

DEFINE_PROC("<")
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

DEFINE_PROC(">")
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

DEFINE_PROC("<=")
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

DEFINE_PROC(">=")
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

DEFINE_PROC("+")
{
    obj_t *p = F_SUBJ;
    int sum = 0;
    while (!is_null(p)) {
	sum += fixnum_value(pair_car(p));
	p = pair_cdr(p);
    }
    RETURN(make_fixnum(sum));
}

DEFINE_PROC("-")
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

DEFINE_PROC("*")
{
    obj_t *p = F_SUBJ;
    int prod = 1;
    while (!is_null(p)) {
	prod *= fixnum_value(pair_car(p));
	p = pair_cdr(p);
    }
    RETURN(make_fixnum(prod));
}

DEFINE_PROC("div")
{
    int dividend = fixnum_value(pair_car(F_SUBJ));
    int divisor = fixnum_value(pair_car(pair_cdr(F_SUBJ)));
    assert(is_null(pair_cdr(pair_cdr(F_SUBJ))));
    RETURN(make_fixnum(dividend / divisor));
}

DEFINE_PROC("mod")
{
    int dividend = fixnum_value(pair_car(F_SUBJ));
    int divisor = fixnum_value(pair_car(pair_cdr(F_SUBJ)));
    assert(is_null(pair_cdr(pair_cdr(F_SUBJ))));
    RETURN(make_fixnum(dividend % divisor));
}

DEFINE_PROC("abs")
{
    assert(is_null(pair_cdr(F_SUBJ)));
    int x = fixnum_value(pair_car(F_SUBJ));
    RETURN(make_fixnum(x < 0 ? -x : x));
}
