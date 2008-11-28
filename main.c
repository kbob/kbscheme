#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "io.h"
#include "eval.h"
#include "lib.h"
#include "print.h"
#include "proc.h"
#include "read.h"
#include "roots.h"
#include "test.h"
#include "types.h"

int main(int argc, char *argv[])
{
    register_procs();
    if (argc == 2 && !strcmp(argv[1], "-t"))
	self_test();
    else {
	FILE *fin = fopen("repl.scm", "r");
	if (!fin) {
	    perror("repl.scm");
	    exit(1);
	}
	instream_t *in = make_file_instream(fin);
	obj_t *repl = micro_read(in);
	eval(repl, library_env(r6rs_base_library()));
	printf("\n");
    }
    return 0;
}
