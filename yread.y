%{
    #include <assert.h>
    #include <stdlib.h>
    #include <string.h>
    #include <unicode.h>
    #include <wctype.h>

    #include "io.h"
    #include "obj.h"
    #include "roots.h"
    #include "types.h"

    typedef obj_t *optr_t;

    #define YYSTYPE optr_t

    static int yylex(YYSTYPE *, instream_t *);
    static void yyerror (instream_t *, obj_t **, char const *s);
    static obj_t *make_list(obj_t *car, obj_t *cadr);

%}

%pure-parser
%parse-param {instream_t *in}
%parse-param {obj_t **opp}
%lex-param {instream_t *in}
%error-verbose

%token EXACT_NUMBER SIMPLE ABBREV COMMENT BEGIN_VECTOR BEGIN_BYTEVECTOR

%%

program  : comment program
         | datum			{ *opp = $1; YYACCEPT; }
         | /* empty */			{ *opp = make_symbol(L"exit"); }
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

static inline bool is_symchar(wchar_t wc)
{
    switch (unicode_type(wc)) {

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

    default:
	return false;
    }
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
	    //  verify "#(vu8" and return BEGIN_BYTEARRAY;

	    case L'!':
		while ((wc = instream_getwc(in)) != WEOF && is_symchar(wc))
		    continue;
		continue;

	    case L'|':
		/* scan until another | followed by # are found,
		   or EOF, which is an error. */
		{
		    int state = 0;
		    while (state != 2) {
			wc = instream_getwc(in);
			if (wc == EOF)
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

	if (is_symchar(wc)) {
	    size_t len = 16, pos = 0;
	    wchar_t *buf = alloca(len * sizeof *buf);
	    instream_ungetwc(wc, in);
	    while ((wc = instream_getwc(in)) != WEOF && is_symchar(wc)) {
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
yyerror (instream_t *in, obj_t **opp, char const *s)
{
    fprintf (stderr, "%s\n", s);
}

obj_t *yyread(instream_t *in)
{
    obj_t *optr;
    int r = yyparse(in, &optr);
    assert(r == 0);
    return optr;
}
