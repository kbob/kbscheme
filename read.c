#include "read.h"

#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <wctype.h>

#include "obj.h"

typedef enum token_type {
    TOK_EOF,
    TOK_LPAREN,
    TOK_RPAREN,
    TOK_FIXNUM,
    TOK_SYMBOL
} token_type_t;

typedef struct token {
    token_type_t  tok_type;
    obj_t        *tok_obj;
} token_t;

static token_t make_token(token_type_t type, obj_t *op)
{
    token_t tok = { type, op ? op : make_null() };
    return tok;
}

static inline bool is_symchar(wchar_t wc)
{
    return iswgraph(wc) && wc != L'(' && wc != L')';
}

static token_t scan(instream_t *ip)
{
    wint_t wc;
    while ((wc = instream_getwc(ip)) != WEOF && iswspace(wc))
	continue;
    if (wc == WEOF) {
	return make_token(TOK_EOF, NULL);
    }
    if (wc == L'(') {
	return make_token(TOK_LPAREN, NULL);
    }
    if (wc == L')') {
	return make_token(TOK_RPAREN, NULL);
    }
    if (iswdigit(wc)) {
	instream_ungetwc(wc, ip);
	int ival = 0;
	while ((wc = instream_getwc(ip)) != WEOF && iswdigit(wc)) {
	    ival = 10 * ival + wctob(wc) - '0';
	}
	if (wc != WEOF)
	    instream_ungetwc(wc, ip);
	return make_token(TOK_FIXNUM, make_fixnum(ival));
    }
    if (iswgraph(wc)) {
	size_t len = 2, pos = 0;
	wchar_t *buf = alloca(len * sizeof *buf);
	instream_ungetwc(wc, ip);
	while ((wc = instream_getwc(ip)) != WEOF && is_symchar(wc)) {
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
	    instream_ungetwc(wc, ip);
	return make_token(TOK_SYMBOL, make_symbol(buf));
    }
    fprintf(stderr, "unexpected char %lc = %d\n", wc, wc);
    assert(0);
}

static obj_t *read_sequence(instream_t *ip);

static obj_t *read_object(instream_t *ip, token_t *tok_out)
{
    *tok_out = scan(ip);
    switch (tok_out->tok_type) {

    case TOK_SYMBOL:
    case TOK_FIXNUM:
	return tok_out->tok_obj;

    case TOK_LPAREN:
	return read_sequence(ip);

    case TOK_RPAREN:
	return make_null();

    case TOK_EOF:
	return make_null();

    default:
	fprintf(stderr, "unexpected input: %d\n", tok_out->tok_type);
	assert(false);
	return make_null();
    }
}

static obj_t *read_sequence(instream_t *ip)
{
    token_t tok;
    obj_t *op = read_object(ip, &tok);
    if (tok.tok_type != TOK_RPAREN && tok.tok_type != TOK_EOF)
	op = make_pair(op, read_sequence(ip));
    return op;
}

obj_t *micro_read(instream_t *ip)
{
    token_t tok;
    obj_t *op = read_object(ip, &tok);
    if (tok.tok_type == TOK_RPAREN) {
	fprintf(stderr, "unexpected close parenthesis\n");
	return make_null();
    } else if (tok.tok_type == TOK_EOF)
	return make_symbol(L"exit");	/* XXX raise an exception? */
    return op;
}
