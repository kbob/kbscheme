#include <assert.h>

#include "extend.h"			/* XXX */
#include "proc.h"

DEFINE_PROC("number?")
{
    assert(is_null(pair_cdr(F_ARGL)));
    RETURN(make_boolean(is_fixnum(pair_car(F_ARGL))));
}

DEFINE_PROC("integer?")
{
    assert(is_null(pair_cdr(F_ARGL)));
    RETURN(make_boolean(is_fixnum(pair_car(F_ARGL))));
}

DEFINE_PROC("=")
{
    int x = fixnum_value(pair_car(F_ARGL));
    while (!is_null(F_ARGL)) {
	if (fixnum_value(pair_car(F_ARGL)) != x)
	    RETURN(make_boolean(false));
	F_ARGL = pair_cdr(F_ARGL);
    }
    RETURN(make_boolean(true));
}

DEFINE_PROC("<")
{
    int x = fixnum_value(pair_car(F_ARGL));
    F_ARGL = pair_cdr(F_ARGL);
    while (!is_null(F_ARGL)) {
	int y = fixnum_value(pair_car(F_ARGL));
	if (!(x < y))
	    RETURN(make_boolean(false));
	x = y;
	F_ARGL = pair_cdr(F_ARGL);
    }
    RETURN(make_boolean(true));
}

DEFINE_PROC(">")
{
    int x = fixnum_value(pair_car(F_ARGL));
    F_ARGL = pair_cdr(F_ARGL);
    while (!is_null(F_ARGL)) {
	int y = fixnum_value(pair_car(F_ARGL));
	if (!(x > y))
	    RETURN(make_boolean(false));
	x = y;
	F_ARGL = pair_cdr(F_ARGL);
    }
    RETURN(make_boolean(true));
}

DEFINE_PROC("<=")
{
    int x = fixnum_value(pair_car(F_ARGL));
    F_ARGL = pair_cdr(F_ARGL);
    while (!is_null(F_ARGL)) {
	int y = fixnum_value(pair_car(F_ARGL));
	if (!(x <= y))
	    RETURN(make_boolean(false));
	x = y;
	F_ARGL = pair_cdr(F_ARGL);
    }
    RETURN(make_boolean(true));
}

DEFINE_PROC(">=")
{
    int x = fixnum_value(pair_car(F_ARGL));
    F_ARGL = pair_cdr(F_ARGL);
    while (!is_null(F_ARGL)) {
	int y = fixnum_value(pair_car(F_ARGL));
	if (!(x >= y))
	    RETURN(make_boolean(false));
	x = y;
	F_ARGL = pair_cdr(F_ARGL);
    }
    RETURN(make_boolean(true));
}

DEFINE_PROC("+")
{
    int sum = 0;
    while (!is_null(F_ARGL)) {
	sum += fixnum_value(pair_car(F_ARGL));
	F_ARGL = pair_cdr(F_ARGL);
    }
    RETURN(make_fixnum(sum));
}

DEFINE_PROC("-")
{
    int diff = fixnum_value(pair_car(F_ARGL));
    F_ARGL = pair_cdr(F_ARGL);
    if (is_null(F_ARGL))
	RETURN(make_fixnum(-diff));
    while (!is_null(F_ARGL)) {
	diff -= fixnum_value(pair_car(F_ARGL));
	F_ARGL = pair_cdr(F_ARGL);
    }
    RETURN(make_fixnum(diff));
}

DEFINE_PROC("*")
{
    int prod = 1;
    while (!is_null(F_ARGL)) {
	prod *= fixnum_value(pair_car(F_ARGL));
	F_ARGL = pair_cdr(F_ARGL);
    }
    RETURN(make_fixnum(prod));
}

DEFINE_PROC("div")
{
    int dividend = fixnum_value(pair_car(F_ARGL));
    int divisor = fixnum_value(pair_car(pair_cdr(F_ARGL)));
    assert(is_null(pair_cdr(pair_cdr(F_ARGL))));
    RETURN(make_fixnum(dividend / divisor));
}

DEFINE_PROC("mod")
{
    int dividend = fixnum_value(pair_car(F_ARGL));
    int divisor = fixnum_value(pair_car(pair_cdr(F_ARGL)));
    assert(is_null(pair_cdr(pair_cdr(F_ARGL))));
    RETURN(make_fixnum(dividend % divisor));
}

DEFINE_PROC("abs")
{
    assert(is_null(pair_cdr(F_ARGL)));
    int x = fixnum_value(pair_car(F_ARGL));
    RETURN(make_fixnum(x < 0 ? -x : x));
}
