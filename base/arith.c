#include "extend.h"

#include <assert.h>

oldDEFINE_PROC("number?")
{
    assert(is_null(pair_cdr(ARGLIST)));
    return make_boolean(is_fixnum(pair_car(ARGLIST)));
}

oldDEFINE_PROC("integer?")
{
    assert(is_null(pair_cdr(ARGLIST)));
    return make_boolean(is_fixnum(pair_car(ARGLIST)));
}

oldDEFINE_PROC("=")
{
    int x = fixnum_value(pair_car(ARGLIST));
    while (!is_null(ARGLIST)) {
	if (fixnum_value(pair_car(ARGLIST)) != x)
	    return make_boolean(false);
	ARGLIST = pair_cdr(ARGLIST);
    }
    return make_boolean(true);
}

oldDEFINE_PROC("<")
{
    int x = fixnum_value(pair_car(ARGLIST));
    ARGLIST = pair_cdr(ARGLIST);
    while (!is_null(ARGLIST)) {
	int y = fixnum_value(pair_car(ARGLIST));
	if (!(x < y))
	    return make_boolean(false);
	x = y;
	ARGLIST = pair_cdr(ARGLIST);
    }
    return make_boolean(true);
}

oldDEFINE_PROC(">")
{
    int x = fixnum_value(pair_car(ARGLIST));
    ARGLIST = pair_cdr(ARGLIST);
    while (!is_null(ARGLIST)) {
	int y = fixnum_value(pair_car(ARGLIST));
	if (!(x > y))
	    return make_boolean(false);
	x = y;
	ARGLIST = pair_cdr(ARGLIST);
    }
    return make_boolean(true);
}

oldDEFINE_PROC("<=")
{
    int x = fixnum_value(pair_car(ARGLIST));
    ARGLIST = pair_cdr(ARGLIST);
    while (!is_null(ARGLIST)) {
	int y = fixnum_value(pair_car(ARGLIST));
	if (!(x <= y))
	    return make_boolean(false);
	x = y;
	ARGLIST = pair_cdr(ARGLIST);
    }
    return make_boolean(true);
}

oldDEFINE_PROC(">=")
{
    int x = fixnum_value(pair_car(ARGLIST));
    ARGLIST = pair_cdr(ARGLIST);
    while (!is_null(ARGLIST)) {
	int y = fixnum_value(pair_car(ARGLIST));
	if (!(x >= y))
	    return make_boolean(false);
	x = y;
	ARGLIST = pair_cdr(ARGLIST);
    }
    return make_boolean(true);
}

oldDEFINE_PROC("+")
{
    int sum = 0;
    while (!is_null(ARGLIST)) {
	sum += fixnum_value(pair_car(ARGLIST));
	ARGLIST = pair_cdr(ARGLIST);
    }
    return make_fixnum(sum);
}

oldDEFINE_PROC("-")
{
    int diff = fixnum_value(pair_car(ARGLIST));
    ARGLIST = pair_cdr(ARGLIST);
    if (is_null(ARGLIST))
	return make_fixnum(-diff);
    while (!is_null(ARGLIST)) {
	diff -= fixnum_value(pair_car(ARGLIST));
	ARGLIST = pair_cdr(ARGLIST);
    }
    return make_fixnum(diff);
}

oldDEFINE_PROC("*")
{
    int prod = 1;
    while (!is_null(ARGLIST)) {
	prod *= fixnum_value(pair_car(ARGLIST));
	ARGLIST = pair_cdr(ARGLIST);
    }
    return make_fixnum(prod);
}

oldDEFINE_PROC("div")
{
    int dividend = fixnum_value(pair_car(ARGLIST));
    int divisor = fixnum_value(pair_car(pair_cdr(ARGLIST)));
    assert(is_null(pair_cdr(pair_cdr(ARGLIST))));
    return make_fixnum(dividend / divisor);
}

oldDEFINE_PROC("mod")
{
    int dividend = fixnum_value(pair_car(ARGLIST));
    int divisor = fixnum_value(pair_car(pair_cdr(ARGLIST)));
    assert(is_null(pair_cdr(pair_cdr(ARGLIST))));
    return make_fixnum(dividend % divisor);
}

oldDEFINE_PROC("abs")
{
    assert(is_null(pair_cdr(ARGLIST)));
    int x = fixnum_value(pair_car(ARGLIST));
    return make_fixnum(x < 0 ? -x : x);
}
