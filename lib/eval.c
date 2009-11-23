#include "eval.h"
#include "proc.h"

LIBRARY(L"(rnrs eval (6))")

/*
 * r6rs-lib 16, eval
 *
 * (eval expression environment)			# procedure
 *
 * (environment import-spec ...)			# procedure
 */

DEFINE_PROC(L"eval")
{
    obj_t *expr = pair_car(F_SUBJ);
    obj_t *env = pair_cadr(F_SUBJ);
    GOTO(b_eval, expr, env);
}
