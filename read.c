#include "read.h"

#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <wctype.h>

#include "obj.h"
#include "roots.h"
#include "scan.h"
#include "test.h"
#include "types.h"

static obj_t *read_object(instream_t *in, token_type_t *tok_out);

static obj_t *read_sequence(instream_t *in)
{
    token_type_t tok;
    AUTO_ROOT(obj, read_object(in, &tok));
    if (tok != TOK_RPAREN && tok != TOK_EOF) {
	AUTO_ROOT(cdr, read_sequence(in));
	obj = make_pair(obj, cdr);
	POP_ROOT(cdr);
    }
    POP_ROOT(obj);
    return obj;
}

static obj_t *read_object(instream_t *in, token_type_t *tok_out)
{
    obj_t *obj;
    *tok_out = yylex(&obj, in);
    switch (*tok_out) {

    case TOK_SIMPLE:
    case TOK_EXACT_NUMBER:
	return obj;

    case TOK_LPAREN:
	return read_sequence(in);

    case TOK_RPAREN:
	return NIL;

    case TOK_EOF:
	return NIL;

    default:
	fprintf(stderr, "unexpected input: %s\n", token_name(*tok_out));
	assert(false);
	return NIL;
    }
}

bool read_stream(instream_t *in, obj_t **obj_out)
{
    token_type_t tok;
    obj_t *obj = read_object(in, &tok);
    if (tok == TOK_RPAREN) {
	fprintf(stderr, "unexpected close parenthesis\n");
	*obj_out = NIL;
	return false;
    } else if (tok == TOK_EOF) {
	*obj_out = NIL;
	return false;
    } else {
	*obj_out = obj;
	return true;
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
//TEST_READ(L"(a#;()b)",                  L"(a b)");
//TEST_READ(L"(a#;(comment)b)",           L"(a b)");
//TEST_READ(L"(a#;(\n)b)",                L"(a b)");
//TEST_READ(L"(a#;\t()b)",                L"(a b)");
//TEST_READ(L"(a#;((c)(d))b)",            L"(a b)");
//TEST_READ(L"(#;c a . b)",		L"(a . b)");
//TEST_READ(L"(#;c#;c a . b)",		L"(a . b)");
//TEST_READ(L"(a#;c . b)",		L"(a . b)");
//TEST_READ(L"(a #;c#;c . b)",		L"(a . b)");
//TEST_READ(L"(a #;c#;c . #;c b)",	L"(a . b)");
//TEST_READ(L"(a #;c . #;c #;c b)",	L"(a . b)");
//TEST_READ(L"(a #;c#;c . #;c b #;c)",	L"(a . b)");
//TEST_READ(L"(a . #;c#;c b#;c#;c)",	L"(a . b)");
//TEST_READ(L"(a#;c . #;c#;c b#;c#;c)",	L"(a . b)");
//TEST_READ(L"(a . #;()#;() b#;()#;())",	L"(a . b)");
TEST_READ(L"(a#!r6rs b)",		L"(a b)");
TEST_READ(L"#!r6rs(a b)",		L"(a b)");
TEST_READ(L"(#!r6rs a b)",		L"(a b)");
TEST_READ(L"(#!r6\x33s a b)",		L"(a b)");

#define TEST_IDENT(name)						\
    TEST_READ(L ## #name, L ## #name);					\
    TEST_EVAL(L"(symbol? '" L ## #name L")", L"#t");

/* identifiers */
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

/*from r6rs section 4.2.4 */
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

/* numbers */

#define TEST_NUMBER(input, expected)					\
    TEST_READ(L ## #input, L ## #expected);					\
    TEST_EVAL(L"(number? " L ## #expected L")", L"#t");
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

/* lists */
TEST_READ(L"(a b)",			L"(a b)");
//TEST_READ(L"#; asdf ghjk",		L"ghjk");
TEST_EVAL(L"(pair? '(a b))",		L"#t");
//TEST_READ(L"[a b]",			L"(a b)");
//TEST_EVAL(L"(pair? '[a b])",		L"#t");
//TEST_READ(L"#(a b)",			L"#(a b)");
//TEST_READ(L"#(a (b c))",		L"#(a (b c))");
//TEST_READ(L"#(a #(b c))",		L"#(a #(b c))");
//TEST_READ(L"#vu8(1 2)",			L"#vu8(1 2)");
