#ifndef PRINT_INCLUDED
#define PRINT_INCLUDED

#include "io.h"
#include "obj.h"

extern void princ(obj_t *, outstream_t *);
extern void print(obj_t *, outstream_t *);

#include "io.h"					/* XXX */
#include <stdio.h>				/* XXX */
static inline void princ_stdout(obj_t *op)	/* XXX */
{						/* XXX */
    princ(op,					/* XXX */
	  make_file_outstream(stdout));		/* XXX */
}						/* XXX */
						/* XXX */
static inline void print_stdout(obj_t *op)	/* XXX */
{						/* XXX */
    princ_stdout(op);				/* XXX */
    printf("\n");				/* XXX */
}						/* XXX */

#endif /* !PRINT_INCLUDED */
