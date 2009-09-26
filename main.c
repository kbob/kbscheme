#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "io.h"
#include "eval.h"
#include "lib.h"
#include "load.h"
#include "mem.h"
#include "print.h"
#include "proc.h"
#include "read.h"
#include "roots.h"
#include "test.h"
#include "types.h"

static char exec_dir[PATH_MAX + 1];

void init_exec_dir(const char *cmd)
{
    const char *p = strrchr(cmd, '/');
    if (p) {
	size_t len = p - cmd;
	if (len >= sizeof exec_dir)
	    return;
	strncpy(exec_dir, cmd, len);
	exec_dir[len] = 0;
    } else {
	ssize_t len = readlink("/proc/self/exe", exec_dir, sizeof exec_dir - 1);
	if (len < 0) {
	    perror("readlink");
	    return;
	}
	exec_dir[len] = '\0';
    }    
    set_exec_path(exec_dir);
}

int main(int argc, char *argv[])
{
    init_exec_dir(argv[0]);
    init_heap();
    init_roots();
    register_libraries();
    register_procs();
    load_scheme(exec_dir);
    load_libraries();
    if (argc == 2 && !strcmp(argv[1], "-t"))
	self_test();
    else {
	FILE *fin = fopen("repl.scm", "r");
	if (!fin) {
	    perror("repl.scm");
	    exit(1);
	}
	instream_t *in = make_file_instream(fin);
	AUTO_ROOT(repl, NIL);
	while (read_stream(in, &repl))
	    eval(repl, library_env(r6rs_library()));
	POP_ROOT(repl);
	delete_instream(in);
	printf("\n");
    }
    return 0;
}
