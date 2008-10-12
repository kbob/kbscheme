#include "proc.h"

DEFINE_PROC("pair?")
{
    RETURN(make_boolean(is_pair(pair_car(F_SUBJ))));
}

DEFINE_PROC("null?")
{
    RETURN(make_boolean(is_null(pair_car(F_SUBJ))));
}

DEFINE_PROC("cons")
{
    RETURN(make_pair(pair_car(F_SUBJ),
		     pair_car(pair_cdr(F_SUBJ))));
}

DEFINE_PROC("car")
{
    RETURN(pair_car(pair_car(F_SUBJ)));
}

DEFINE_PROC("cdr")
{
    RETURN(pair_cdr(pair_car(F_SUBJ)));
}
