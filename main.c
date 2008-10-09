#include <stdio.h>
#include <string.h>

#include "io.h"
#include "eval.h"
#include "lib.h"
#include "print.h"
#include "read.h"
#include "test.h"

int main(int argc, char *argv[])
{
    if (argc == 2 && !strcmp(argv[1], "-t"))
	self_test();
    else {
	instream_t *ip = make_readline_instream();
	outstream_t *out = make_file_outstream(stdout);
	while (true) {
	    obj_t *op = micro_read(ip);
	    if (is_symbol(op) &&
		!wcscmp(string_value(symbol_name(op)), L"exit"))
		break;
	    op = eval(op, library_env(r6rs_base_library()));
	    print(op, out);
	}
	printf("\n");
    }
    return 0;
}
