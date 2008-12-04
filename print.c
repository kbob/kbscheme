#include "print.h"

#include <assert.h>
#include <limits.h>

#include "obj_binding.h"
#include "types.h"

/* XXX Reimplement this in Scheme. */

static void print_form(obj_t *, outstream_t *);

static void print_list_interior(obj_t *obj, wchar_t *sep, outstream_t *out)
{
    while (!is_null(obj)) {
	outstream_printf(out, sep);
	print_form(pair_car(obj), out);
	obj = pair_cdr(obj);
	if (!is_null(obj) && !is_pair(obj)) {
	    outstream_printf(out, L" . ");
	    print_form(obj, out);
	    break;
	}
	sep = L" ";
    }
}

static void print_pair(obj_t *obj, outstream_t *out)
{
    outstream_putwc(L'(', out);
    print_list_interior(obj, L"", out);
    outstream_putwc(L')', out);
}

static void print_procedure(obj_t *obj, outstream_t *out)
{
    if (procedure_is_special_form(obj) || procedure_is_C(obj)) {
	outstream_printf(out, L"#<proc-%s%s>",
			 procedure_is_special_form(obj) ? "S" : "",
			 procedure_is_C(obj) ? "C" : "");
    } else {
	outstream_printf(out, L"(lambda ");
	print_form(procedure_args(obj), out);
	print_list_interior(procedure_body(obj), L" ", out);
	outstream_printf(out, L")");
    }
}

static void print_binding(obj_t *obj, outstream_t *out)
{
    print_form(binding_name(obj), out);
    outstream_putwc(binding_is_mutable(obj) ? L':' : L'!', out);
    print_form(binding_value(obj), out);
}

static void print_form(obj_t *obj, outstream_t *out)
{
    if (is_null(obj) || is_pair(obj)) {
	print_pair(obj, out);
    } else if (is_boolean(obj)) {
	outstream_printf(out, boolean_value(obj) ? L"#t" : L"#f");
    } else if (is_fixnum(obj)) {
	outstream_printf(out, L"%d", fixnum_value(obj));
    } else if (is_character(obj)) {
	/* implement me */
	assert(false && "can't print characters yet");
    } else if (is_string(obj)) {
	/* implement me */
	assert(false && "can't print strings yet");
    } else if (is_symbol(obj)) {
	outstream_printf(out, L"%ls", string_value(symbol_name(obj)));
    } else if (is_procedure(obj)) {
	print_procedure(obj, out);
    } else if (is_binding(obj)) {
	print_binding(obj, out);
    } else {
	outstream_printf(out, L"#<%ls-%p>", object_type_name(obj), obj);
    }
}

void princ(obj_t *obj, outstream_t *out)
{
     print_form(obj, out);
}

static bool is_unspecified(obj_t *obj)
{
    return is_symbol(obj) &&
	!wcscmp(string_value(symbol_name(obj)), UNSPECIFIED_NAME);
}

void print(obj_t *obj, outstream_t *out)
{
    if (!is_unspecified(obj)) {
	print_form(obj, out);
	outstream_putwc(L'\n', out);
    }
}
