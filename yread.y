%{
    #include "read.h"

    #include <assert.h>
    #include <stdlib.h>
    #include <string.h>
    #include <unicode.h>
    #include <wctype.h>

    #include "io.h"
    #include "obj.h"
    #include "roots.h"
    #include "test.h"
    #include "types.h"

    typedef obj_t *optr_t;

    #define YYSTYPE optr_t

    static int yylex(YYSTYPE *, instream_t *);
    static void yyerror (instream_t *, obj_t **, bool *, char const *s);
    static obj_t *make_list(obj_t *car, obj_t *cadr);
%}

%pure-parser
%parse-param {instream_t *in}
%parse-param {obj_t **opp}
%parse-param {bool *reached_eof}
%lex-param   {instream_t *in}
%error-verbose

%token EXACT_NUMBER SIMPLE ABBREV COMMENT BEGIN_VECTOR BEGIN_BYTEVECTOR

%%

program  : comment program
         | datum			{ *opp = $1; YYACCEPT;    }
         | /* empty */			{ *reached_eof = true;    }
         ;

datum    : simple
         | compound
         ;

simple   : EXACT_NUMBER
         | SIMPLE
         ;

compound : '(' sequence ')'		{ $$ = $2;                }
         | '[' sequence ']'		{ $$ = $2;                }
      /* | BEGIN_VECTOR sequence ')'    { $$ = make_vector($2);   } */
      /* | BEGIN_BYTEVECTOR bytes ')'   { $$ = make bytevec($2);  } */
         | ABBREV datum                 { $$ = make_list($1, $2); }
         ;

sequence : datum sequence		{ $$ = make_pair($1, $2); }
         | comment sequence		{ $$ = $2;                }
         | datum '.' datum		{ $$ = make_pair($1, $3); }
         | /* empty */			{ $$ = NIL;               }
         ;

//bytes    : bytes EXACT_NUMBER
//         | /* empty */
//         ;

comment  : COMMENT datum
         ;

%%

static obj_t *make_list(obj_t *car, obj_t *cadr)
{
    PUSH_ROOT(car);
    obj_t *list = make_pair(car, make_pair(cadr, NIL));
    POP_ROOT(car);
    return list;
}

static inline bool is_ident_initial(wchar_t wc)
{
    /* Ruthless elision of braces is fun! */
    if (wc < 128)
	return iswalpha(wc) || wcschr(L"!$%&*/:<=>?^_~", wc);
    else
	switch (unicode_type(wc))
	case UNICODE_PRIVATE_USE:
	case UNICODE_LOWERCASE_LETTER:
	case UNICODE_MODIFIER_LETTER:
	case UNICODE_OTHER_LETTER:
	case UNICODE_TITLECASE_LETTER:
	case UNICODE_UPPERCASE_LETTER:
	case UNICODE_COMBINING_MARK:
	case UNICODE_ENCLOSING_MARK:
	case UNICODE_NON_SPACING_MARK:
	case UNICODE_DECIMAL_NUMBER:
	case UNICODE_LETTER_NUMBER:
	case UNICODE_OTHER_NUMBER:
	case UNICODE_CONNECT_PUNCTUATION:
	case UNICODE_DASH_PUNCTUATION:
	case UNICODE_OTHER_PUNCTUATION:
	case UNICODE_CURRENCY_SYMBOL:
	case UNICODE_MODIFIER_SYMBOL:
	case UNICODE_MATH_SYMBOL:
	case UNICODE_OTHER_SYMBOL:
	    return true;
    return false;
}

static inline bool is_ident_subsequent(wchar_t wc)
{
    if (is_ident_initial(wc) || iswdigit(wc) || wcschr(L"+-.@", wc))
	return true;
    switch (unicode_type(wc)) {
    case UNICODE_DECIMAL_NUMBER:
    case UNICODE_COMBINING_MARK:
    case UNICODE_ENCLOSING_MARK:
	return true;
    }
    return false;
}

static int yylex(YYSTYPE *lvalp, instream_t *in)
{
    wint_t wc;

    while ((wc = instream_getwc(in)) != WEOF) {
	if (iswspace(wc))
	    continue;
	if (wc == L';') {
	    while ((wc = instream_getwc(in)) != WEOF &&
		   !wcschr(L"\r\n\x0085\x2028\x2029", wc))
		continue;
	    continue;
	}
	if (wcschr(L"()[].", wc)) {
	    return wctob(wc);
	}
	if (wc == L'\'') {
	    *lvalp = make_symbol(L"quote");
	    return ABBREV;
	}
	if (wc == L'`') {
	    *lvalp = make_symbol(L"quasiquote");
	    return ABBREV;
	}
	if (wc == L',') {
	    wc = instream_getwc(in);
	    if (wc == '@') {
		*lvalp = make_symbol(L"unquote-splicing");
		return ABBREV;
	    }
	    instream_ungetwc(wc, in);
	    *lvalp = make_symbol(L"unquote");
	    return ABBREV;
	}
	if (wc == L'#') {
	    /* #t #f #( #vu8( #| #; #' #` #, #,@ #!r6rs */
	    wc = instream_getwc(in);
	    switch (wc) {

	    case L'T':
	    case L't':
		*lvalp = make_boolean(true);
		return SIMPLE;

	    case L'F':
	    case L'f':
		*lvalp = make_boolean(false);
		return SIMPLE;

	    case L'(':
	      return BEGIN_VECTOR;    

	    //case L'v':
	    //  verify "#vu8(" and return BEGIN_BYTEARRAY;

	    case L'!':
		if ((wc = instream_getwc(in)) != WEOF && is_ident_initial(wc))
		    while ((wc = instream_getwc(in)) != WEOF &&
			   is_ident_subsequent(wc))
			continue;
		continue;

	    case L'|':
		/* scan until another | followed by # are found,
		   or EOF, which is an error. */
		{
		    int state = 0;
		    while (state != 2) {
			wc = instream_getwc(in);
			if (wc == WEOF)
			    assert(0 && "unterminated block comment");
			if (wc == L'|')
			    state = 1;
			else if (state == 1)
			    state = (wc == L'#') ? 2 : 0;
		    }
		}
		continue;

	    case L';':
		return COMMENT;

	    case L'\'':
		*lvalp = make_symbol(L"syntax");
		return ABBREV;

	    case L'`':
		*lvalp = make_symbol(L"quasisyntax");
		return ABBREV;

	    case L',':
		wc = instream_getwc(in);
		if (wc == '@') {
		    *lvalp = make_symbol(L"unsyntax-splicing");
		    return ABBREV;
		}
		instream_ungetwc(wc, in);
		*lvalp = make_symbol(L"unsyntax");
		return ABBREV;

	    default:
		/* fall through and fail */
		instream_ungetwc(wc, in);
		wc = '#';
	    }
	}
	if (wc == L'+' || wc == L'-' || iswdigit(wc)) {
	    int sign = +1;
	    if (wc == L'-')
		sign = -1;
	    else if (wc != L'+')
		instream_ungetwc(wc, in);
	    int ival = 0;
	    bool ok = false;
	    while ((wc = instream_getwc(in)) != WEOF && iswdigit(wc)) {
		ok = true;
		ival = 10 * ival + (wc & 0xF);
	    }
	    if (wc != WEOF)
		instream_ungetwc(wc, in);
	    if (ok) {
		*lvalp = make_fixnum(sign * ival);
		return EXACT_NUMBER;
	    } else {
		*lvalp = make_symbol(sign < 0 ? L"-" : L"+");
		return SIMPLE;
	    }
	}

	if (is_ident_initial(wc)) {
	    size_t len = 16, pos = 0;
	    wchar_t *buf = alloca(len * sizeof *buf);
	    instream_ungetwc(wc, in);
	    while ((wc = instream_getwc(in)) != WEOF &&
		   is_ident_subsequent(wc)) {
		if (pos >= len - 1) {
		    int nbytes = (len *= 2) * sizeof *buf;
		    wchar_t *tmp = alloca(nbytes);
		    assert(tmp);
		    memmove(tmp, buf, nbytes);
		    buf = tmp;
		}
		buf[pos++] = btowc(wc);
	    }
	    buf[pos] = L'\0';
	    if (wc != WEOF)
		instream_ungetwc(wc, in);
	    *lvalp = make_symbol(buf);
	    return SIMPLE;
	}
	fprintf(stderr, "unexpected char L'\\x%08x' = %d\n", wc, wc);
	assert(0);
    }
    return 0;				/* EOF */
}

static void
yyerror (instream_t *in, obj_t **opp, bool *reached_eof, char const *s)
{
    fprintf (stderr, "%s\n", s);
}

bool read_stream(instream_t *in, obj_t **obj_out)
{
    bool reached_eof = false;
    *obj_out = NIL;
    int r = yyparse(in, obj_out, &reached_eof);
    assert(r == 0);
    return !reached_eof;
}

/* spaces */
TEST_READ(L"(a b)",          L"(a b)");
TEST_READ(L"(a\tb)",         L"(a b)");
TEST_READ(L"(a\fb)",         L"(a b)");
TEST_READ(L"(a;comment\nb)", L"(a b)");

/* numbers */
TEST_READ(L"0",              L"0");
TEST_EVAL(L"(number? 0)",    L"#t");
TEST_READ(L"+12", L"12");
TEST_EVAL(L"(number? +12)",  L"#t");
TEST_READ(L"-23", L"-23");
TEST_EVAL(L"(number? -23)",  L"#t");
//TEST_READ(L"#i0", L"0");
//TEST_READ(L"#I0", L"0");
//TEST_READ(L"#e0", L"0");
//TEST_READ(L"#E0", L"0");
//TEST_READ(L"#b101", L"5");
//TEST_READ(L"#o77", L"63");
//TEST_READ(L"#e#b101", L"5");
//TEST_READ(L"0.1", L"0.1");
//TEST_READ(L"#e0.1", L"1/10");

/* lists */
TEST_READ(L"(a b)", L"(a b)");
TEST_READ(L"[a b]", L"(a b)");
//TEST_READ(L"#(a b)", L"#(a b)");
