/*
 * First draft implementations.
 */

#include <stdio.h>

#include "io.h"
#include "print.h"
#include "proc.h"
#include "read.h"

static instream_t  *in;
static outstream_t *out;

DEFINE_PROC(L"draft-read")
{
    /* with lock */ {
	if (!in)
	    in = make_readline_instream();
    }
    RETURN(micro_read(in));
}

DEFINE_PROC(L"draft-print")
{
    /* with lock */ {
	if (!out)
	    out = make_file_outstream(stdout);
    }
    print(pair_car(F_SUBJ), out);
    RETURN(NIL);
}

DEFINE_PROC(L"draft-current-environment")
{
    RETURN(F_ENV);
}
