#include "proc.h"

#define MAKE_b_continue_if_FRAME_ make_short_frame
DEFINE_BLOCK(b_continue_if)
{
    obj_t *truth = F_VAL;
    if (!is_boolean(truth) || boolean_value(truth)) {
	obj_t *consequent = pair_car(F_SUBJ);
	TAIL_EVAL(consequent, F_ENV);
    }
    obj_t *cddr = pair_cdr(F_SUBJ);
    if (is_null(cddr))
	RETURN(cddr);
    obj_t *alternate = pair_car(cddr);
    TAIL_EVAL(alternate, F_ENV);
}

DEFINE_SPECIAL_FORM("if")
{
    obj_t *test = pair_car(F_SUBJ);
    obj_t *subj_cdr = pair_cdr(F_SUBJ);
    EVAL_THEN_GOTO(test, F_ENV, b_continue_if, subj_cdr, F_ENV);
}
