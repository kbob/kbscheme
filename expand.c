#include "expand.h"

#include "proc.h"

LIBRARY(L"(foo)");			// XXX won't need this

DEFINE_PROC(L"expand")			/* initial implementation */
{
    RETURN(pair_car(F_SUBJ));
}

obj_t *expander(void)
{
    obj_t *expand_sym = make_symbol_from_C_str(L"expand");
    obj_t *expand_binding = env_lookup(root_environment(), expand_sym);
    return binding_value(expand_binding);
}
