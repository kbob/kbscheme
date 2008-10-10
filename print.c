#include "print.h"

#include <assert.h>
#include <limits.h>

/* XXX Reimplement this in Scheme. */

static void print_form(obj_t *, outstream_t *);

static void put_string(const wchar_t *p, outstream_t *out)
{
    while (*p) {
	outstream_putwc(*p, out);
	p++;
    }
}

static void print_fixnum(int i, int base, outstream_t *out)
{
    assert(0 < base && base <= 16);
    if (i < 0) {
	outstream_putwc(L'-', out);
	i = -i;
	if (i < 0) {
	    assert(INT_MIN == -1 << 31);
	    put_string(L"2147483648", out);
	    return;
	}
    }
    if (i >= base) {
	print_fixnum(i / base, base, out);
    }
    outstream_putwc(L"0123456789abcdef"[i % base], out);
}

static void print_list_interior(obj_t *op, wchar_t *sep, outstream_t *out)
{
    while (!is_null(op)) {
	put_string(sep, out);
	print_form(pair_car(op), out);
	op = pair_cdr(op);
	if (!is_null(op) && !is_pair(op)) {
	    put_string(L". ", out);
	    print_form(op, out);
	    break;
	}
	sep = L" ";
    }
}

static void print_pair(obj_t *op, outstream_t *out)
{
    outstream_putwc(L'(', out);
    print_list_interior(op, L"", out);
    outstream_putwc(L')', out);
}

static void print_procedure(obj_t *op, outstream_t *out)
{
    if (procedure_is_special_form(op) || procedure_is_C(op)) {
	put_string(L"#<proc-", out);
	if (procedure_is_special_form(op))
	    outstream_putwc(L'S', out);
	if (procedure_is_C(op))
	    outstream_putwc(L'C', out);
	// outstream_putwc(L' ', out);
	// print_fixnum((int) op, 0x10, out);
	outstream_putwc(L'>', out);
    } else {
	put_string(L"(lambda ", out);
	print_form(procedure_args(op), out);
	print_list_interior(procedure_body(op), L" ", out);
	put_string(L")", out);
    }
}

static void print_form(obj_t *op, outstream_t *out)
{
    if (is_null(op) || is_pair(op)) {
	print_pair(op, out);
    } else if (is_boolean(op)) {
	put_string(boolean_value(op) ? L"#t" : L"#f", out);
    } else if (is_fixnum(op)) {
	print_fixnum(fixnum_value(op), 10, out);
    } else if (is_character(op)) {
	/* implement me */
	assert(false && "can't print characters yet");
    } else if (is_string(op)) {
	/* implement me */
	assert(false && "can't print strings yet");
    } else if (is_symbol(op)) {
	put_string(string_value(symbol_name(op)), out);
    } else if (is_procedure(op)) {
	print_procedure(op, out);
    } else {
	fprintf(stderr, "object type %d\n", * (char *) op);
	assert(false && "unknown object");
    }
}

void princ(obj_t *op, outstream_t *out)
{
     print_form(op, out);
}

void print(obj_t *op, outstream_t *out)
{
    if (!is_null(op)) {
	print_form(op, out);
	outstream_putwc(L'\n', out);
    }
}
