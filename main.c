#include <stdio.h>
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
	instream_t *ip = make_readline_instream();
	outstream_t *out = make_file_outstream(stdout);
	while (true) {
	    AUTO_ROOT(obj, micro_read(ip));
	    if (is_symbol(obj) &&
		!wcscmp(string_value(symbol_name(obj)), L"exit"))
		break;
	    obj = eval(obj, library_env(r6rs_base_library()));
	    print(obj, out);
	    POP_ROOT(obj);
	}
	printf("\n");
    }
    return 0;
}
