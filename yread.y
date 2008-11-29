%{
    #include <assert.h>
    #include <ctype.h>
    #include <stdio.h>
    #include <stdlib.h>
    #include <string.h>
    #include <wchar.h>
    #include <wctype.h>

    #include "io.h"
    #include "obj.h"
    #include "print.h"
    #include "read.h"
    #include "types.h"

    typedef obj_t *optr_t;

    #define YYSTYPE optr_t

    static int yylex(YYSTYPE *, instream_t *);
    static void yyerror (instream_t *, obj_t **, char const *s);
%}

%pure-parser
%parse-param {instream_t *in}
%parse-param {obj_t **opp}
%lex-param {instream_t *in}
%error-verbose

%token FIXNUM SYMBOL

%initial-action {
    printf("initial action\n");
}

%%

start: form { printf("match start:form\n"); *opp = $1; };

form: FIXNUM { printf("match form: FIXNUM\n"); }
    | SYMBOL { printf("match form: SYMBOL\n"); }
    | '(' {} seq ')' { printf("match form: ( seq )\n"); $$ = $2; }
    ;

seq :
    | seq form
    ;

%%

static int yylex(YYSTYPE *lvalp, instream_t *in)
{
    wint_t wc;

    while ((wc = instream_getwc(in)) != WEOF && iswspace(wc))
	continue;
    if (wc == WEOF) {
	printf("yylex return EOF\n");
	return 0;
    }
    if (wcschr(L"()", wc)) {
	printf("yylex return '%c'\n", wctob(wc));
	return wctob(wc);
    }
    if (iswdigit(wc)) {
	instream_ungetwc(wc, in);
	int ival = 0;
	while ((wc = instream_getwc(in)) != WEOF && iswdigit(wc)) {
	    ival = 10 * ival + wctob(wc) - '0';
	}
	instream_ungetwc(wc, in);
	*lvalp = make_fixnum(ival);
	printf("yylex return FIXNUM %d\n", ival);
	return FIXNUM;
    }
    
    if (iswgraph(wc)) {
	size_t len = 2, pos = 0;
	wchar_t *buf = malloc(len * sizeof *buf);
	instream_ungetwc(wc, in);
	while ((wc = instream_getwc(in)) != WEOF &&
	       iswgraph(wc) && !wcschr(L"()", wc)) {
	    if (pos >= len - 1) {
		wchar_t *tmp = realloc(buf, (len *= 2) * sizeof *buf);
		assert(tmp);
		buf = tmp;
	    }
	    buf[pos++] = btowc(wc);
	}
	buf[pos] = L'\0';
	instream_ungetwc(wc, in);
	*lvalp = make_symbol(buf);
	printf("yylex return SYMBOL %ls\n", buf);
	free(buf);
	return SYMBOL;
    }
    fprintf(stderr, "unexpected char L'\\x%08x' = %d\n", wc, wc);
    assert(0);
}

static void
yyerror (instream_t *in, obj_t **opp, char const *s)
{
    fprintf (stderr, "%s\n", s);
}

obj_t *YYYmicro_read(instream_t *in)
{
    obj_t *optr;
    int r = yyparse(in, &optr);
    printf("yyparse returned %d\n", r);
    assert(r == 0);
    printf("optr=%p\n", optr);
    printf("*optr=0x%x\n", * (int *) optr);
    return optr;
}
