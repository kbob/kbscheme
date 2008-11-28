/*
 * First draft implementations.
 */

#include "io.h"
#include "print.h"
#include "proc.h"
#include "read.h"

static instream_t  *in;
static outstream_t *out;

/* XXX These should be in an invisible namespace. */

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

DEFINE_PROC(L"draft-environment")
{
    RETURN(library_env(r6rs_base_library()));
    RETURN(F_ENV);
}
