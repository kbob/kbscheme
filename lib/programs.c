#include <stdlib.h>
#include <string.h>

#include "main.h"
#include "proc.h"

LIBRARY(L"(rnrs programs (6))")

obj_t *make_string_from_narrow_C_str(const char *p)
{
    size_t i, len = strlen(p);
    obj_t *str = make_string(len, L'\0');
    for (i = 0; i < len; i++)
	string_set_char(str, i, (wchar_t)p[i]);
    return str;
}

DEFINE_PROC(L"command-line")
{
    const char **p;
    for (p = main_argv; *p; p++)
	continue;
    AUTO_ROOT(l, NIL);
    while (--p >= main_argv) {
	obj_t *arg = make_string_from_narrow_C_str(*p);
	l = make_pair(arg, l);
    }
    RETURN(l);
}

DEFINE_PROC(L"exit")
{
    int status;
    if (is_null(F_SUBJ))
	status = 0;
    else {
	obj_t *p = pair_car(F_SUBJ);
	if (is_boolean(p))
	    status = !boolean_value(p);
	else if (is_fixnum(p))
	    status = fixnum_value(p);
	else
	    status = 2;
    }
    /* XXX raise a SystemExit exception. */
    exit(status);
}
