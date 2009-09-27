#include "load.h"

#include <errno.h>
#include <limits.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "eval.h"
#include "io.h"
#include "obj.h"
#include "proc.h"
#include "read.h"

/*
 * The path is stored on the C stack, then freed immediately after
 * opening.
 */

static FILE *open_file(const char *dir_path, const char *file_name)
{
    char path[PATH_MAX];
    if (snprintf(path, sizeof path,
		 "%s/%s", dir_path, file_name) >= sizeof path) {
	errno = ENOMEM;
	return NULL;
    }
    return fopen(path, "r");
}

void load_scheme(const char *dir_path)
{
    FILE *f = open_file(dir_path, "scheme.scm");
    if (!f) {
	perror("scheme.scm");
	exit(1);
    }
    instream_t *in = make_file_instream(f);
    obj_t *form;
    while (read_stream(in, &form)) {
	eval(form, root_environment());
    }    
    delete_instream(in);
    fclose(f);
}
