#ifndef PRINT_INCLUDED
#define PRINT_INCLUDED

#include "io.h"
#include "obj.h"

extern void princ(obj_t *, outstream_t *);
extern void print(obj_t *, outstream_t *);

#include <stdio.h>				/* XXX */
						/* XXX */
static inline void princ_stdout(obj_t *obj)	/* XXX */
{						/* XXX */
    static outstream_t *out = NULL;		/* XXX */
    if (!out)					/* XXX */
	out = make_file_outstream(stdout);	/* XXX */
    princ(obj, out);				/* XXX */
}						/* XXX */
						/* XXX */
static inline void print_stdout(obj_t *obj)	/* XXX */
{						/* XXX */
    princ_stdout(obj);				/* XXX */
    printf("\n");				/* XXX */
}						/* XXX */

#endif /* !PRINT_INCLUDED */
