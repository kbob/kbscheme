#include "expand.h"

#include <stdio.h>
#include <stdlib.h>

#include "eval.h"
#include "io.h"
#include "lib.h"
#include "obj_pair.h"
#include "obj_procedure.h"
#include "read.h"
#include "roots.h"

STATIC_ROOT(expander_proc);

static void load_expander(void)
{
    FILE *fin = fopen("expand.scm", "r");
    if (!fin) {
	perror("expand.scm");
	exit(1);
    }
    AUTO_ROOT(proc, NIL);
    instream_t *in = make_file_instream(fin);
    if (!read_stream(in, &proc))
	fprintf(stderr, "error in expand.scm\n");
    delete_instream(in);
    obj_t *lib = r6rs_library();
    obj_t *params = pair_cadr(proc);
    obj_t *body = pair_cddr(proc);
    expander_proc = make_special_form_procedure(body, params, lib);
    POP_ROOT(proc);
}

obj_t *expander(void)
{
    if (!expander_proc)
	load_expander();
    return expander_proc;
}
