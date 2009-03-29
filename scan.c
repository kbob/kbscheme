#include "scan.h"

#include <assert.h>
#include <string.h>
#include <unicode.h>
#include <wctype.h>

#include "types.h"

static inline bool is_whitespace(wchar_t wc)
{
    switch (unicode_type(wc)) {
    case UNICODE_LINE_SEPARATOR:
    case UNICODE_SPACE_SEPARATOR:
    case UNICODE_PARAGRAPH_SEPARATOR:
	return true;
    }
    return (bool)wcschr(L"\t\n\v\f\r\x85", wc);
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
    if (is_ident_initial(wc) || wcschr(L"+-.@", wc))
	return true;
    switch (unicode_type(wc)) {
    case UNICODE_DECIMAL_NUMBER:
    case UNICODE_COMBINING_MARK:
    case UNICODE_ENCLOSING_MARK:
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

static wchar_t inline_hex_escape(instream_t *in)
{
    int xval = 0;
    wchar_t wc;
    while ((wc = instream_getwc(in)) != WEOF && is_xdigit(wc))
	xval = 0x10 * xval + digit_value(wc);
    assert(wc == L';');
    return (wchar_t)xval;
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

static token_type_t scan_ident(const wchar_t *prefix,
			       obj_t **lvalp,
			       instream_t *in)
{
    wchar_t wc;
    size_t len = 16, pos = wcslen(prefix);
    assert(pos < len);
    wchar_t *buf = alloca(len * sizeof *buf);
    wcscpy(buf, prefix);
    while (true) {
	wc = instream_getwc(in);
	if (wc == WEOF)
	    break;
	if (wc == L'\\') {
	    wc = instream_getwc(in);
	    if (wc == L'x' || wc == L'X') {
		wc = inline_hex_escape(in);
	    } else if (wc != WEOF) {
		instream_ungetwc(wc, in);
		break;
	    }
	} else if (!is_ident_subsequent(wc))
	    break;
	if (pos >= len - 1) {
	    int nbytes = (len *= 2) * sizeof *buf;
	    wchar_t *tmp = alloca(nbytes);
	    assert(tmp);
	    memmove(tmp, buf, nbytes);
	    buf = tmp;
	}
	buf[pos++] = wc;
    }
    buf[pos] = L'\0';
    if (wc != WEOF)
	instream_ungetwc(wc, in);
    *lvalp = make_symbol(buf);
    return TOK_SIMPLE;
}

extern token_type_t yylex(obj_t **lvalp, instream_t *in)
{
    wint_t wc, w2;

    while ((wc = instream_getwc(in)) != WEOF) {
	if (is_whitespace(wc))
	    continue;
	if (wc == L';') {
	    while ((wc = instream_getwc(in)) != WEOF && !is_line_ending(wc))
		continue;
	    continue;
	}
	if (wcschr(L"()[]", wc)) {
	    return (token_type_t)wctob(wc);
	}
	if (wc == L'.') {
	    /* . is a token.
             * ... is an identifier.
             * Anything else is an error. */
	    int n = 1;
	    while ((w2 = instream_getwc(in)) == L'.')
		n++;
	    if (w2 != WEOF)
		instream_ungetwc(w2, in);
	    if (!is_ident_subsequent(w2)) {
		if (n == 1)
		    return (token_type_t)wctob(L'.');
		if (n == 3) {
		    *lvalp = make_symbol(L"...");
		    return TOK_SIMPLE;
		}
	    }
	    /* fall through to ignominy. */
	}
	if (wc == L'\'') {
	    *lvalp = make_symbol(L"quote");
	    return TOK_ABBREV;
	}
	if (wc == L'`') {
	    *lvalp = make_symbol(L"quasiquote");
	    return TOK_ABBREV;
	}
	if (wc == L',') {
	    w2 = instream_getwc(in);
	    if (w2 == L'@') {
		*lvalp = make_symbol(L"unquote-splicing");
		return TOK_ABBREV;
	    }
	    instream_ungetwc(w2, in);
	    *lvalp = make_symbol(L"unquote");
	    return TOK_ABBREV;
	}
	if (wc == L'#') {
	    /* #t #f #( #vu8( #| #; #' #` #, #,@ #!r6rs */
	    /* XXX implement #\<any character> */
	    /* XXX implement #\<character name> */
	    /* XXX implement #\x<hex scalar value> */
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
		    scan_ident(L"#!", lvalp, in);
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
			else if (w2 == L'|' && state == 2) {
			    state = 0;
			    depth++;
			} else if (w2 == L'#' && state == 0)
			    state = 2;
			else if (w2 == L'#' && state == 1) {
			    state = 0;
			    --depth;
			}
		    }
		}
		continue;

	    case L';':
		return TOK_COMMENT;

	    case L'\'':
		*lvalp = make_symbol(L"syntax");
		return TOK_ABBREV;

	    case L'`':
		*lvalp = make_symbol(L"quasisyntax");
		return TOK_ABBREV;

	    case L',':
		w2 = instream_getwc(in);
		if (w2 == L'@') {
		    *lvalp = make_symbol(L"unsyntax-splicing");
		    return TOK_ABBREV;
		}
		instream_ungetwc(w2, in);
		*lvalp = make_symbol(L"unsyntax");
		return TOK_ABBREV;
	    }
	    /* fall through to failure. */
	    instream_ungetwc(wc, in);
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
		*lvalp = make_symbol(L"-");
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
		*lvalp = make_symbol(L"+");
		return TOK_SIMPLE;
	    }
	    if (!is_ident_subsequent(w2)) {
		instream_ungetwc(w2, in);
		*lvalp = make_symbol(L"+");
		return TOK_SIMPLE;
	    }
	    if (is_digit(w2)) {
		instream_ungetwc(w2, in);
		return scan_number(+1, lvalp, in);
	    }
	}
	if (is_digit(wc)) {
	    instream_ungetwc(wc, in);
	    return scan_number(+1, lvalp, in);
	}
	if (wc == L'\\') {
	    w2 = instream_getwc(in);
	    if (w2 == L'x' || w2 == L'X') {
		wchar_t prefix[2] = { inline_hex_escape(in), L'\0' };
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
