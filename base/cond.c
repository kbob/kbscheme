#include "proc.h"

#include "eval.h"

DEFINE_SPECIAL_FORM("if")
{
    obj_t *test = pair_car(F_SUBJ);
    obj_t *truth = eval(test, F_ENV);
    obj_t *cdr = pair_cdr(F_SUBJ);
    if (!is_boolean(truth) || boolean_value(truth)) {
	obj_t *consequent = pair_car(cdr);
	TAIL_EVAL(consequent, F_ENV);
    } else {
	obj_t *cddr = pair_cdr(cdr);
	if (is_null(cddr))
	    RETURN(cddr);
	else {
	    obj_t *alternate = pair_car(cddr);
	    TAIL_EVAL(alternate, F_ENV);
	}
    }    
}
