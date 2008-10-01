#include "extend.h"

#include "eval.h"

DEFINE_SYNTAX("if")
{
    obj_t *test = pair_car(ARGLIST);
    obj_t *truth = eval(test, ENV);
    obj_t *cdr = pair_cdr(ARGLIST);
    if (!is_boolean(truth) || boolean_value(truth)) {
	obj_t *consequent = pair_car(cdr);
	return eval(consequent, ENV);
    } else {
	obj_t *cddr = pair_cdr(cdr);
	if (is_null(cddr))
	    return cddr;
	else {
	    obj_t *alternate = pair_car(cddr);
	    return eval(alternate, ENV);
	}
    }    
}
