/* Register new printf handlers to print objects. */
/* Not used until the heap is initialized. */

#include <assert.h>
#include <printf.h>

#include "io.h"
#include "print.h"

static int print_obj(FILE *stream,
		     const struct printf_info *info,
		     const void *const *args)
{
    outstream_t *out = make_file_outstream(stream);
    long a = outstream_pos(out);
    obj_t *obj = *(obj_t **)args[0];
    princ(obj, out);
    long b = outstream_pos(out);
    delete_outstream(out);
    return b - a;
}

static int obj_arginfo(const struct printf_info *info, size_t n, int *argtypes)
{
    if (n > 0)
	argtypes[0] = PA_POINTER;
    return 1;
}

__attribute__((constructor))
static void extend_printf(void)
{
    register_printf_function('O', print_obj, obj_arginfo);
}
