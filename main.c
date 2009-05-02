#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "io.h"
#include "eval.h"
#include "lib.h"
#include "mem.h"
#include "print.h"
#include "proc.h"
#include "read.h"
#include "roots.h"
#include "test.h"
#include "types.h"

int main(int argc, char *argv[])
{
    init_heap();
    init_roots();
    register_libraries();
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
	obj_t *repl;
	while (read_stream(in, &repl))
	    eval(repl, library_env(r6rs_library()));
	delete_instream(in);
	printf("\n");
    }
    return 0;
}
