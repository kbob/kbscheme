#include "io.h"

#include <assert.h>
#include <limits.h>
#include <stdlib.h>
#include <readline/readline.h>
#include <readline/history.h>

typedef wint_t (*in_getwc_proc_t)(instream_t *);
typedef wint_t (*in_ungetwc_proc_t)(wint_t, instream_t *);
typedef wint_t (*out_putwc_proc_t)(wchar_t, outstream_t *);

struct instream {
    in_getwc_proc_t   in_getwc;
    in_ungetwc_proc_t in_ungetwc;
};

typedef struct file_instream {
    instream_t        fi_instream;
    FILE             *fi_file;
} file_instream_t;

typedef struct readline_instream {
    instream_t        ri_instream;
    wchar_t          *ri_linebuf;
    size_t            ri_pos;
    wchar_t           ri_ungot;
    mbstate_t         ri_ps;
} readline_instream_t;

struct outstream {
    out_putwc_proc_t  out_putwc;
};

typedef struct file_outstream {
    outstream_t       fo_outstream;
    FILE             *fo_file;
} file_outstream_t;

static void init_instream(instream_t *ip,
			  in_getwc_proc_t get,
			  in_ungetwc_proc_t unget)
{
    ip->in_getwc = get;
    ip->in_ungetwc = unget;
}

static void init_outstream(outstream_t *out, out_putwc_proc_t put)
{
    out->out_putwc = put;
}

/* file instream methods */

static wint_t file_getwc(instream_t *ip)
{
    file_instream_t *fp = (file_instream_t *) ip;
    return fgetwc(fp->fi_file);
}

static wint_t file_ungetwc(wint_t wc, instream_t *ip)
{
    file_instream_t *fp = (file_instream_t *) ip;
    return ungetwc(wc, fp->fi_file);
}

instream_t *make_file_instream(FILE *f)
{
    file_instream_t *fp = (file_instream_t *) malloc(sizeof *fp);
    instream_t *ip = &fp->fi_instream;
    init_instream(ip, file_getwc, file_ungetwc);
    fp->fi_file = f;
    return ip;
}

/* readline instream methods */

static wint_t readline_getwc(instream_t *ip)
{
    readline_instream_t *rp = (readline_instream_t *) ip;
    wint_t wc;
    size_t line_len;
    size_t nb;

    if (rp->ri_ungot != WEOF) {
	wc = rp->ri_ungot;
	rp->ri_ungot = WEOF;
	return wc;
    }
    if (rp->ri_linebuf == NULL) {
	const char *linebuf = readline("> ");
	const char *tmp = linebuf;
	mbstate_t mbstate;
	if (!linebuf) {
	    return WEOF;
	}
	memset(&mbstate, '\0', sizeof mbstate);
	line_len = mbsrtowcs(NULL, &linebuf, 0, &mbstate);
	assert(line_len >= 0);
	rp->ri_linebuf = malloc((line_len + 1) * sizeof *rp->ri_linebuf);
	tmp = linebuf;
	nb = mbsrtowcs(rp->ri_linebuf, &tmp, line_len + 1, &mbstate);
	assert(nb >= 0);
	rp->ri_pos = 0;
    }
    line_len = wcslen(rp->ri_linebuf);
    if (rp->ri_pos < line_len) {
	wc = rp->ri_linebuf[rp->ri_pos++];
    } else {
	free(rp->ri_linebuf);
	rp->ri_linebuf = NULL;
	wc = L'\n';
    }
    return wc;
}

static wint_t readline_ungetwc(wint_t wc, instream_t *ip)
{
    readline_instream_t *rp = (readline_instream_t *) ip;
    assert(rp->ri_ungot == WEOF);
    assert(wc != WEOF);
    rp->ri_ungot = wc;
    return wc;
}

instream_t *make_readline_instream(void)
{
    readline_instream_t *rp = (readline_instream_t *) malloc(sizeof *rp);
    instream_t *ip = &rp->ri_instream;
    init_instream(ip, readline_getwc, readline_ungetwc);
    rp->ri_linebuf = NULL;
    rp->ri_pos = 0;
    rp->ri_ungot = WEOF;
    return ip;
}

/* file outstream methods */

static wint_t file_putwc(wchar_t wc, outstream_t *out)
{
    file_outstream_t *fp = (file_outstream_t *) out;
    if (fwide(fp->fo_file, 0) > 0) {
	return fputwc(wc, fp->fo_file);
    } else {
	char buf[MB_LEN_MAX];
	int r, nb = wctomb(buf, wc);
	if (nb < 0)
	    return WEOF;
	r = fwrite(buf, nb, 1, fp->fo_file);
	return (r == 1) ? wc : WEOF;
    }
}

outstream_t *make_file_outstream(FILE *f)
{
    file_outstream_t *fp = (file_outstream_t *) malloc(sizeof *fp);
    outstream_t *out = &fp->fo_outstream;
    init_outstream(out, file_putwc);
    fp->fo_file = f;
    return out;
}

/* generic methods */

wint_t instream_getwc(instream_t *ip)
{
    return ip->in_getwc(ip);
}

wint_t instream_ungetwc(wint_t wc, instream_t *ip)
{
    return ip->in_ungetwc(wc, ip);
}

wint_t outstream_putwc(wchar_t wc, outstream_t *out)
{
    return out->out_putwc(wc, out);
}
