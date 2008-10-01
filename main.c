#include <stdio.h>

#include "io.h"
#include "eval.h"
#include "extend.h"
#include "print.h"
#include "read.h"

int main()
{
    instream_t *ip = make_readline_instream();
    outstream_t *out = make_file_outstream(stdout);
    while (true) {
	obj_t *op = micro_read(ip);
	if (is_symbol(op) && !wcscmp(string_value(symbol_name(op)), L"exit"))
	    break;
	op = eval(op, library_env(r6rs_base_library()));
	print(op, out);
    }
    printf("\n");
    return 0;
}
