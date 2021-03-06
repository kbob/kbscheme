#include "scan.h"

#include <assert.h>
#include <string.h>
#include <wctype.h>

#include "charbuf.h"
#include "test.h"
#include "types.h"
#include "unicode.h"

typedef struct char_name_map {
    wchar_t *cn_name;
    wchar_t  cn_char;
} char_name_map_t;

static char_name_map_t char_names[] = {
    { L"alarm",     L'\a'   },
    { L"backspace", L'\b'   },
    { L"delete",    L'\177' },
    { L"esc",       L'\33'  },
    { L"linefeed",  L'\n'   },
    { L"newline",   L'\n'   },		/* deprecated */
    { L"nul",       L'\0'   },
    { L"page",      L'\f'   },
    { L"return",    L'\r'   },
    { L"space",     L' '    },
    { L"tab",       L'\t'   },
    { L"vtab",      L'\v'   },
};

static size_t char_name_count = sizeof char_names / sizeof *char_names;

static inline bool is_intraline_whitespace(wchar_t wc)
{
    return wc == '\t' || unicode_general_category(wc) == UGC_SEPARATOR_SPACE;
}

static inline bool is_whitespace(wchar_t wc)
{
    switch ((int)unicode_general_category(wc))
    case UGC_SEPARATOR_LINE:
    case UGC_SEPARATOR_SPACE:
    case UGC_SEPARATOR_PARAGRAPH:
	return true;
    return (bool)wcschr(L"\t\n\v\f\r\x85", wc);
}

static inline bool is_delimiter(wc)
{
    return (bool)wcschr(L"()[]\";#", wc) || is_whitespace(wc);
}

static inline bool is_line_ending(wchar_t wc)
{
    return (bool)wcschr(L"\r\n\x0085\x2028\x2029", wc);
}

static inline bool is_digit(wchar_t wc)
{
    return L'0' <= wc && wc <= L'9';
}

static inline bool is_xdigit(wchar_t wc)
{
    if (is_digit(wc))
	return true;
    return (L'a' <= wc && wc <= L'f') || (L'A' <= wc && wc <= L'F');
}

static inline bool is_ident_initial(wchar_t wc)
{
    /* Ruthless elision of braces is fun! */
    if (wc < 128)
	return iswalpha(wc) || wcschr(L"!$%&*/:<=>?^_~", wc);
    else
	switch ((int)unicode_general_category(wc))
	case UGC_OTHER_PRIVATE_USE:
	case UGC_LETTER_LOWERCASE:
	case UGC_LETTER_MODIFIER:
	case UGC_LETTER_OTHER:
	case UGC_LETTER_TITLECASE:
	case UGC_LETTER_UPPERCASE:
	case UGC_MARK_NONSPACING:
	case UGC_NUMBER_LETTER:
	case UGC_NUMBER_OTHER:
	case UGC_PUNCTUATION_CONNECTOR:
	case UGC_PUNCTUATION_DASH:
	case UGC_PUNCTUATION_OTHER:
	case UGC_SYMBOL_CURRENCY:
	case UGC_SYMBOL_MODIFIER:
	case UGC_SYMBOL_MATH:
	case UGC_SYMBOL_OTHER:
	    return true;
    return false;
}

static inline bool is_ident_subsequent(wchar_t wc)
{
    if (is_ident_initial(wc) || wcschr(L"+-.@", wc))
	return true;
    switch ((int)unicode_general_category(wc)) {
    case UGC_NUMBER_DECIMAL_DIGIT:
    case UGC_MARK_SPACING_COMBINING:
    case UGC_MARK_ENCLOSING:
	return true;
    }
    return false;
}

static int digit_value(wchar_t wc)
{
    if (L'0' <= wc && wc <= L'9')
	return wc - L'0';
    if (L'a'<= wc && wc <= L'f')
	return wc - L'a' + 0xa;
    assert(L'A'<= wc && wc <= L'F');
    return wc - L'A' + 0xA;
}

static bool inline_hex_scalar(instream_t *in, wchar_t *xchr, wchar_t end)
{
    int xval = 0;
    wchar_t wc;
    while ((wc = instream_getwc(in)) != WEOF && is_xdigit(wc))
	xval = 0x10 * xval + digit_value(wc);
    if (end) {
	if (wc == WEOF || wc != end)
	    return false;
    } else if (wc != WEOF)
	instream_ungetwc(wc, in);
    if (xval > 0x10ffff || (xval >= 0xd800 && xval <= 0xdfff))
	return false;
    *xchr = (wchar_t)xval;
    return true;
}

static token_type_t scan_ident(const wchar_t *prefix,
			       obj_t **lvalp,
			       instream_t *in)
{
    wchar_t wc;
    charbuf_t buf;
    init_charbuf(&buf, prefix);
    while (true) {
	wc = instream_getwc(in);
	if (wc == WEOF)
	    break;
	if (wc == L'\\') {
	    wc = instream_getwc(in);
	    if (wc == L'x' || wc == L'X') {
		if (!inline_hex_scalar(in, &wc, L';'))
		    assert(false && "bad hex scalar");
	    } else if (wc != WEOF) {
		instream_ungetwc(wc, in);
		break;
	    }
	} else if (!is_ident_subsequent(wc))
	    break;
	charbuf_append_char(&buf, wc);
    }
    if (wc != WEOF)
	instream_ungetwc(wc, in);
    *lvalp = make_symbol(charbuf_make_string(&buf));
    return TOK_SIMPLE;
}

static bool scan_character(obj_t **lvalp, instream_t *in)
{
    /* Accumulate [A-Za-z0-9]* up to delimiter.  */
    wint_t wc = instream_getwc(in);
    wchar_t wchr;
    if (wc == WEOF)
	return false;
    if (wc == L'x') {
	wint_t w2 = instream_getwc(in);
	instream_ungetwc(w2, in);
	if (iswxdigit(w2)) {
	    if (!inline_hex_scalar(in, &wchr, 0))
		return false;
	    w2 = instream_getwc(in);
	    if (w2 != WEOF) {
		if (!is_delimiter(w2))
		    return false;
		instream_ungetwc(w2, in);
	    }
	    *lvalp = make_character(wchr);
	    return true;
	}
    }
    charbuf_t buf;
    init_charbuf(&buf, L"");
    while (true) {
	charbuf_append_char(&buf, wc);
	wc = instream_getwc(in);
	if (wc == WEOF)
	    break;
	if (is_delimiter(wc)) {
	    instream_ungetwc(wc, in);
	    break;
	}
    }    
    if (charbuf_len(&buf) == 1)
	wchr = charbuf_C_str(&buf)[0];
    else {
	size_t i;
	for (i = 0; ; i++) {
	    if (i == char_name_count)
		return false;
	    if (!wcscmp(charbuf_C_str(&buf), char_names[i].cn_name)) {
		wchr = char_names[i].cn_char;
		break;
	    }
	}
    }
    *lvalp = make_character(wchr);
    return true;
}

static token_type_t scan_number(int sign, obj_t **lvalp, instream_t *in)
{
    wchar_t wc;
    int ival = 0;
    while ((wc = instream_getwc(in)) != WEOF && is_digit(wc))
	ival = 10 * ival + (wc & 0xF);
    if (wc != WEOF)
	instream_ungetwc(wc, in);
    *lvalp = make_fixnum(sign * ival);
    return TOK_EXACT_NUMBER;
}

extern token_type_t yylex(obj_t **lvalp, instream_t *in)
{
    wint_t wc, w2;

    *lvalp = NIL;
    while ((wc = instream_getwc(in)) != WEOF) {
	if (is_whitespace(wc))
	    continue;
	if (wc == L';') {
	    while ((wc = instream_getwc(in)) != WEOF && !is_line_ending(wc))
		continue;
	    continue;
	}
	if (wcschr(L"()[]", wc)) {
	    switch (wc) {
	    case L'(':
		return TOK_LPAREN;
	    case L')':
		return TOK_RPAREN;
	    case L'[':
		return TOK_LBRACKET;
	    case L']':
		return TOK_RBRACKET;
	    default:
		assert(0);
	    }
	}
	if (wc == L'.') {
	    /* . is a token.
             * ... is an identifier.
             * Anything else is an error. */
	    /* XXX .foo is legal.  '.' is category Po. */
	    int n = 1;
	    while ((w2 = instream_getwc(in)) == L'.')
		n++;
	    if (w2 != WEOF)
		instream_ungetwc(w2, in);
	    if (!is_ident_subsequent(w2)) {
		if (n == 1)
		    return TOK_PERIOD;
		if (n == 3) {
		    *lvalp = make_symbol_from_C_str(L"...");
		    return TOK_SIMPLE;
		}
	    }
	    /* fall through to ignominy. */
	}
	if (wc == L'\'') {
	    *lvalp = make_symbol_from_C_str(L"quote");
	    return TOK_ABBREV;
	}
	if (wc == L'`') {
	    *lvalp = make_symbol_from_C_str(L"quasiquote");
	    return TOK_ABBREV;
	}
	if (wc == L',') {
	    w2 = instream_getwc(in);
	    if (w2 == L'@') {
		*lvalp = make_symbol_from_C_str(L"unquote-splicing");
		return TOK_ABBREV;
	    }
	    instream_ungetwc(w2, in);
	    *lvalp = make_symbol_from_C_str(L"unquote");
	    return TOK_ABBREV;
	}
	if (wc == L'#') {
	    /* #t #f #( #vu8( #| #; #' #` #, #,@ #!r6rs */
	    /* XXX implement #i... and #e... */
	    wc = instream_getwc(in);
	    switch (wc) {

	    case L'T':
	    case L't':
		*lvalp = make_boolean(true);
		return TOK_SIMPLE;

	    case L'F':
	    case L'f':
		*lvalp = make_boolean(false);
		return TOK_SIMPLE;

	    case L'(':			
		return TOK_BEGIN_VECTOR;

	    case L'v':			/* #vu8( */
		
		if ((w2 = instream_getwc(in)) == L'u' &&
		    (w2 = instream_getwc(in)) == L'8' &&
		    (w2 = instream_getwc(in)) == L'(')
		    return TOK_BEGIN_BYTEVECTOR;
		if (w2 != WEOF)
		    instream_ungetwc(w2, in);
		/* fall through to disgrace. */
		break;

	    case L'!':			/* #!<identifier> */
		if ((w2 = instream_getwc(in)) != WEOF &&
		    is_ident_initial(w2)) {
		    instream_ungetwc(w2, in);
		    obj_t *unused;
		    (void)scan_ident(L"#!", &unused, in);
		    continue;
		}
		instream_ungetwc(w2, in);
		/* fall through to illegitimacy. */
		break;

	    case L'|':
		/* scan until another | followed by # are found,
		   or EOF, which is an error. */
		{
		    int state = 0, depth = 1;
		    while (depth) {
			w2 = instream_getwc(in);
			if (w2 == WEOF)
			    assert(0 && "unterminated block comment");
			if (w2 == L'|' && state == 0)
			    state = 1;
			else if (w2 == L'|' && state == 1) {
			    /* no change */
			} else if (w2 == L'|' && state == 2) {
			    state = 0;
			    depth++;
			} else if (w2 == L'#' && state == 0)
			    state = 2;
			else if (w2 == L'#' && state == 1) {
			    state = 0;
			    --depth;
			} else
			    state = 0;
		    }
		}
		continue;

	    case L';':
		return TOK_COMMENT;

	    case L'\'':
		*lvalp = make_symbol_from_C_str(L"syntax");
		return TOK_ABBREV;

	    case L'`':
		*lvalp = make_symbol_from_C_str(L"quasisyntax");
		return TOK_ABBREV;

	    case L',':
		w2 = instream_getwc(in);
		if (w2 == L'@') {
		    *lvalp = make_symbol_from_C_str(L"unsyntax-splicing");
		    return TOK_ABBREV;
		}
		instream_ungetwc(w2, in);
		*lvalp = make_symbol_from_C_str(L"unsyntax");
		return TOK_ABBREV;

	    case L'\\':			/* #\<character> */
		if (scan_character(lvalp, in))
		    return TOK_SIMPLE;
		wc = instream_getwc(in);
		break;			/* fall through to degeneracy */
	    }
	    /* fall through to failure. */
	    wc = L'#';
	}
	if (wc == L'-') {
	    /*
	     * - is an identifier.
	     * -> is an identifier.
	     * ->foo is an identifier.
	     * -1, -.1 -#b1 are numbers.
	     * Anything else is an error.
	     */
	    w2 = instream_getwc(in);
	    if (w2 != WEOF)
		instream_ungetwc(w2, in);
	    if (w2 == WEOF || !is_ident_subsequent(w2)) {
		*lvalp = make_symbol_from_C_str(L"-");
		return TOK_SIMPLE;
	    }
	    if (w2 == L'.' || is_digit(w2))
		return scan_number(-1, lvalp, in);
	    if (w2 == L'>')
		return scan_ident(L"-", lvalp, in);
	}
	if (wc == L'+') {
	    w2 = instream_getwc(in);
	    if (w2 == EOF) {
		*lvalp = make_symbol_from_C_str(L"+");
		return TOK_SIMPLE;
	    }
	    if (!is_ident_subsequent(w2)) {
		instream_ungetwc(w2, in);
		*lvalp = make_symbol_from_C_str(L"+");
		return TOK_SIMPLE;
	    }
	    if (is_digit(w2)) {
		instream_ungetwc(w2, in);
		return scan_number(+1, lvalp, in);
	    }
	}
	if (wc == L'"') {
	    charbuf_t buf;
	    init_charbuf(&buf, L"");
	    while (true) {
		w2 = instream_getwc(in);
		if (w2 == WEOF)
		    assert(false && "unterminated string");
		if (w2 == L'"')
		    break;
		if (w2 == L'\\') {
		    w2 = instream_getwc(in);
		    switch (w2) {
		    case L'a': w2 = L'\a';  break;
		    case L'b': w2 = L'\b';  break;
		    case L't': w2 = L'\t';  break;
		    case L'n': w2 = L'\n';  break;
		    case L'v': w2 = L'\v';  break;
		    case L'f': w2 = L'\f';  break;
		    case L'r': w2 = L'\r';  break;
		    case L'"': w2 = L'"';   break;
		    case L'\\': w2 = L'\\'; break;
		    case L'x': 
		    case L'X':
			if (!inline_hex_scalar(in, (wchar_t *)&w2, L';'))
			    assert(false && "bad hex escape");
			break;
		    default:
			while (is_intraline_whitespace(w2)) {
			    w2 = instream_getwc(in);
			    if (w2 == WEOF)
				assert(false && "unterminated string");
			}
			if (!is_line_ending(w2))
			    assert(false && "bad backslash escape");
			w2 = instream_getwc(in);
			while (is_intraline_whitespace(w2)) {
			    w2 = instream_getwc(in);
			    if (w2 == WEOF)
				assert(false && "unterminated string");
			}
			instream_ungetwc(w2, in);
			continue;
		    }
		}
		charbuf_append_char(&buf, w2);
	    }
	    *lvalp = charbuf_make_string(&buf);
	    return TOK_SIMPLE;
	}
	if (is_digit(wc)) {
	    instream_ungetwc(wc, in);
	    return scan_number(+1, lvalp, in);
	}
	if (wc == L'\\') {
	    w2 = instream_getwc(in);
	    wchar_t w3;
	    if ((w2 == L'x' || w2 == L'X') &&
		inline_hex_scalar(in, &w3, L';'))
	    {	wchar_t prefix[2] = { w3, L'\0' };
		return scan_ident(prefix, lvalp, in);
	    } else if (w2 != WEOF)
		instream_ungetwc(w2, in);
	    /* fall through into purgatory */
	}
	if (is_ident_initial(wc)) {
	    instream_ungetwc(wc, in);
	    return scan_ident(L"", lvalp, in);
	}
	fprintf(stderr, "unexpected char L'\\x%08x' = %d = %lc\n", wc, wc, wc);
	assert(0);
    }
    return TOK_EOF;
}

const char *token_name(token_type_t tok)
{
    switch (tok) {
#define CASE(x) case (x): return #x;
	CASE(TOK_EOF)
	CASE(TOK_EXACT_NUMBER)
	CASE(TOK_SIMPLE)
	CASE(TOK_ABBREV)
	CASE(TOK_COMMENT)
	CASE(TOK_BEGIN_VECTOR)
	CASE(TOK_BEGIN_BYTEVECTOR)
	CASE(TOK_LPAREN)
	CASE(TOK_RPAREN)
	CASE(TOK_PERIOD)
	CASE(TOK_LBRACKET)
	CASE(TOK_RBRACKET)
#undef CASE
    default:
	assert(false && "unknown token type");
    }
}

/* spaces */
TEST_READ(L"(a b)",                     L"(a b)");
TEST_READ(L"(a\tb)",                    L"(a b)");
TEST_READ(L"(a\vb)",                    L"(a b)");
TEST_READ(L"(a\fb)",                    L"(a b)");
TEST_READ(L"(a\rb)",                    L"(a b)");
TEST_READ(L"(a\x0085"L"b)",             L"(a b)"); /* <next line> */
TEST_READ(L"(a\x2028"L"b)",             L"(a b)"); /* <line separator> */
TEST_READ(L"(a\x2029"L"b)",             L"(a b)"); /* <paragraph separator> */
TEST_READ(L"(a\x00a0"L"b)",             L"(a b)"); /* category Zs */
TEST_READ(L"(a\x2028"L"b)",             L"(a b)"); /* category Zl */
TEST_READ(L"(a\x2029"L"b)",             L"(a b)"); /* category Zp */

/* comments */
TEST_READ(L"(a;comment\nb)",            L"(a b)");
TEST_READ(L"(a;comment\rb)",            L"(a b)");
TEST_READ(L"(a;comment\r\nb)",          L"(a b)");
TEST_READ(L"(a;comment\x0085"L"b)",     L"(a b)");
TEST_READ(L"(a;comment\r\x0085"L"b)",   L"(a b)");
TEST_READ(L"(a;comment\r\nb)",          L"(a b)");
TEST_READ(L"(a;comment\x2028"L"b)",     L"(a b)");
TEST_READ(L"(a#||#b)",                  L"(a b)");
TEST_READ(L"(a#|comment|#b)",           L"(a b)");
TEST_READ(L"(a#|||#b)",                 L"(a b)");
TEST_READ(L"(a#|#||#|#b)",              L"(a b)");
TEST_READ(L"(a#|#|comment|#|#b)",       L"(a b)");
TEST_READ(L"(a#|#||#comment|#b)",       L"(a b)");

/* identifiers */
#define TEST_IDENT(name)						\
    TEST_READ(L ## #name, L ## #name);					\
    TEST_EVAL(L"(symbol? '" L ## #name L")", L"#t");
TEST_IDENT(a);
TEST_IDENT(A);
TEST_IDENT(!);
TEST_IDENT($);
TEST_IDENT(%);
TEST_IDENT(&);
TEST_IDENT(*);
TEST_IDENT(/);
TEST_IDENT(:);
TEST_IDENT(<);
TEST_IDENT(=);
TEST_IDENT(>);
TEST_IDENT(?);
TEST_IDENT(^);
TEST_IDENT(_);
TEST_IDENT(~);
TEST_IDENT(\x0102);			/* category Ll */
TEST_IDENT(\x0101);			/* category Lu */
TEST_IDENT(\x01c5);			/* category Lt */
TEST_IDENT(\x02b0);			/* category Lm */
TEST_IDENT(\x01bb);			/* category Lo */
TEST_IDENT(\x0300);			/* category Mn */
TEST_IDENT(\x2163);			/* category Nl */
TEST_IDENT(\x00bc);			/* category No */
TEST_IDENT(\x301c);			/* category Pd */
TEST_IDENT(\x2040);			/* category Pc */
TEST_IDENT(\x055e);			/* category Po */
TEST_IDENT(\x0e3f);			/* category Sc */
TEST_IDENT(\x208a);			/* category Sm */
TEST_IDENT(\x02c2);			/* category Sk */
TEST_IDENT(\x0482);			/* category So */
TEST_IDENT(\xe000);			/* category Co */
TEST_IDENT(aaA!$%&*/:<=>?^_~);
TEST_IDENT(abc\x0102);			/* category Ll */
TEST_IDENT(abc\x0101);			/* category Lu */
TEST_IDENT(abc\x01c5);			/* category Lt */
TEST_IDENT(abc\x02b0);			/* category Lm */
TEST_IDENT(abc\x01bb);			/* category Lo */
TEST_IDENT(abc\x0300);			/* category Mn */
TEST_IDENT(abc\x2163);			/* category Nl */
TEST_IDENT(abc\x00bc);			/* category No */
TEST_IDENT(abc\x301c);			/* category Pd */
TEST_IDENT(abc\x2040);			/* category Pc */
TEST_IDENT(abc\x055e);			/* category Po */
TEST_IDENT(abc\x0e3f);			/* category Sc */
TEST_IDENT(abc\x208a);			/* category Sm */
TEST_IDENT(abc\x02c2);			/* category Sk */
TEST_IDENT(abc\x0482);			/* category So */
TEST_IDENT(abc\xe000);			/* category Co */
TEST_IDENT(a123);
TEST_IDENT(abc\x0660);			/* category Nd */
TEST_IDENT(abc\x0903);			/* category Mc */
TEST_IDENT(abc\x20dd);			/* category Me */
TEST_IDENT(a+-.@);
TEST_IDENT(+);
TEST_IDENT(-);
TEST_IDENT(...);
TEST_IDENT(->);
TEST_IDENT(->abc);
TEST_READ(L"(->)",			L"(->)");
TEST_READ(L"\\x61;",			L"a");
TEST_READ(L"\\X61;\\X3BB;",		L"a\x3bb");
TEST_READ(L"\\X61;\\X00;\\X3BB;",	L"a\0\x3bb");

/* from r6rs section 4.2.4 */
TEST_IDENT(lambda);
TEST_IDENT(q);
TEST_IDENT(soup);
TEST_IDENT(list->vector);
TEST_IDENT(+);
TEST_IDENT(V17a);
TEST_IDENT(<=);
TEST_IDENT(a34kTMNs);
TEST_IDENT(->-);
TEST_IDENT(the-word-recursion-has-many-meanings);
TEST_READ(L"\\x3BB;",			L"\x3bb");

/* characters */
#define TEST_CHAR(input, expected)					\
    TEST_READ(L"#\\" input, expected);					\
    TEST_EVAL(L"(char? '#\\" input L")", L"#t");
#define TEST_CHAR_LEXICAL_EXCEPTION(input) TEST_READ(L"#\\" input, &lexical)

/* from r6rs section 4.2.6 */
TEST_CHAR(L"a",				L"#\\a");
TEST_CHAR(L"A",				L"#\\A");
TEST_CHAR(L"(",				L"#\\(");
TEST_CHAR(L" ",				L"#\\space");
TEST_CHAR(L"nul",			L"#\\x0000");
TEST_CHAR(L"alarm",			L"#\\x0007");
TEST_CHAR(L"backspace",			L"#\\x0008");
TEST_CHAR(L"tab",			L"#\\x0009");
TEST_CHAR(L"linefeed",			L"#\\x000a");
TEST_CHAR(L"newline",			L"#\\x000a");
TEST_CHAR(L"vtab",			L"#\\x000b");
TEST_CHAR(L"page",			L"#\\x000c");
TEST_CHAR(L"return",			L"#\\x000d");
TEST_CHAR(L"esc",			L"#\\x001b");
TEST_CHAR(L"space",			L"#\\space");
TEST_CHAR(L"delete",			L"#\\x007f");
TEST_CHAR(L"xFF",			L"#\\\x00ff");
TEST_CHAR(L"x03BB",			L"#\\\x03bb");
TEST_CHAR(L"x00006587",			L"#\\\x6587");
TEST_CHAR(L"\x03bb",			L"#\\\x03bb");
TEST_CHAR_LEXICAL_EXCEPTION(L"x0001z");
TEST_CHAR_LEXICAL_EXCEPTION(L"\x03bbx");
TEST_CHAR_LEXICAL_EXCEPTION(L"alarmx");
TEST_READ(L"#\\alarm x",		L"#\\x0007");
TEST_CHAR_LEXICAL_EXCEPTION(L"Alarm");
TEST_CHAR_LEXICAL_EXCEPTION(L"alert");
TEST_CHAR(L"xA",			L"#\\x000a");
TEST_CHAR(L"xFF",			L"#\\\x00ff");
TEST_CHAR(L"xff",			L"#\\\x00ff");
TEST_READ(L"#\\x ff",			L"#\\x");
TEST_READ(L"#\\x(ff)",			L"#\\x");
TEST_CHAR_LEXICAL_EXCEPTION(L"(x)");
TEST_CHAR_LEXICAL_EXCEPTION(L"(x");
TEST_READ(L"#\\((x)",			L"#\\(");
TEST_CHAR_LEXICAL_EXCEPTION(L"x00110000");
TEST_CHAR(L"x000000001",		L"#\\x0001");
TEST_CHAR_LEXICAL_EXCEPTION(L"xD800");

/* strings */
#define TEST_STRING(input, expected)					\
    TEST_READ(L ## input, L ## expected);				\
    TEST_EVAL(L"(string? " L ## input L")", L"#t");
#define TEST_STRING_LEXICAL_EXCEPTION(input) TEST_READ(L ## input, &lexical)

TEST_STRING("\"foo\"", "\"foo\"");
TEST_STRING("\"\"", "\"\"");
TEST_STRING("\"\\a\\b\\t\\n\\v\\f\\r\\\"\\\\\"", "\"\a\b\t\n\v\f\r\"\\\"");
TEST_STRING("\"a\\x3bb;b\"", "\"a\x03bb" L"b\"");
TEST_STRING("\"a \\ \n b\"", "\"a b\"");

/* from r6rs section 4.2.7 */
TEST_STRING("\"abc\"",			"\"abc\"");
TEST_STRING("\"\\x41;bc\"",		"\"Abc\"");
TEST_STRING("\"\\x41; bc\"",		"\"A bc\"");
TEST_STRING("\"\\x41bc;\"",		"\"\x41bc\"");
TEST_STRING_LEXICAL_EXCEPTION("\"\\x41\"");
TEST_STRING_LEXICAL_EXCEPTION("\"\\x;\"");
TEST_STRING_LEXICAL_EXCEPTION("\"\\x41bx;\"");
TEST_STRING("\"\\x00000041;\"",		"\"A\"");
TEST_STRING("\"\\x0010ffff;\"",		"\"\x10ffff\"");
TEST_STRING_LEXICAL_EXCEPTION("\"\\x00000001\"");
TEST_STRING_LEXICAL_EXCEPTION("\"\\xD800;\"");
TEST_STRING("\"A\nbc\"",			"\"A\nbc\"");

/* numbers */
#define TEST_NUMBER(input, expected)					\
    TEST_READ(L ## #input, L ## #expected);				\
    TEST_EVAL(L"(number? " L ## #input L")", L"#t");
TEST_NUMBER(0,       0);
TEST_NUMBER(+12,    12);
TEST_NUMBER(-23,   -23);
//TEST_NUMBER(#i0,   0);
//TEST_NUMBER(#I0,   0);
//TEST_NUMBER(#e0,   0);
//TEST_NUMBER(#E0,   0);
//TEST_NUMBER(#b101, 5);
//TEST_NUMBER(#i0,   0);
//TEST_NUMBER(#i0,   0);
//TEST_NUMBER(#i0,   0);
//TEST_NUMBER(#i0,   0);
//TEST_READ(L"#i0",                       L"0");
//TEST_READ(L"#I0",                       L"0");
//TEST_READ(L"#e0",                       L"0");
//TEST_READ(L"#E0",                       L"0");
//TEST_READ(L"#b101",                     L"5");
//TEST_READ(L"#o77",                      L"63");
//TEST_READ(L"#e#b101",                   L"5");
//TEST_READ(L"0.1",                       L"0.1");
//TEST_READ(L"#e0.1",                     L"1/10");
