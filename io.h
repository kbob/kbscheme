/*
 *
 */

#ifndef IO_INCLUDED
#define IO_INCLUDED

#include <stddef.h>			/* for wchar_t */
#include <stdio.h>			/* for FILE    */
#include <wchar.h>			/* for wint_t  */

typedef struct instream instream_t;
typedef struct outstream outstream_t;

extern instream_t *make_file_instream(FILE *);
extern instream_t *make_readline_instream(void);
extern instream_t *make_string_instream(const wchar_t *, size_t);
extern wint_t instream_getwc(instream_t *);
extern wint_t instream_ungetwc(wint_t, instream_t *);

extern outstream_t *make_file_outstream(FILE *);
extern outstream_t *make_string_outstream(wchar_t *, size_t);
extern wint_t outstream_putwc(wchar_t, outstream_t *);
extern int outstream_printf(outstream_t *, const wchar_t *fmt, ...);

#endif /* !IO_INCLUDED */
