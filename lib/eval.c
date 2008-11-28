#include "eval.h"
#include "proc.h"

DEFINE_PROC(L"eval")
{
    obj_t *expr = pair_car(F_SUBJ);
    obj_t *env = pair_car(pair_cdr(F_SUBJ));
    GOTO(b_eval, expr, env);
}
