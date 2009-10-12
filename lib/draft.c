/*
 * First draft implementations.
 */

#include <assert.h>

#include "io.h"
#include "obj_eof.h"
#include "obj_syntax.h"
#include "print.h"
#include "proc.h"
#include "read.h"
#include "uprintf.h"

LIBRARY(L"(draft)") /* XXX These should be in an invisible namespace. */

static instream_t  *in;

DEFINE_PROC(L"draft-read")
{
    AUTO_ROOT(obj, NIL);
    /* with lock */ {
	if (!in)
	    in = make_readline_instream();
	if (!read_stream(in, &obj))
	    obj = make_eof();
    }
    RETURN(obj);
}

DEFINE_PROC(L"draft-print")
{
    if (pair_cdr(F_SUBJ)) {
	int precision = fixnum_value(pair_cadr(F_SUBJ));
	printf_unchecked("%.*O\n", precision, pair_car(F_SUBJ));
    } else {
	printf_unchecked("%O\n", pair_car(F_SUBJ));
    }
    RETURN(UNSPECIFIED);
}

DEFINE_PROC(L"draft-environment")
{
    RETURN(library_env(r6rs_library()));
}

#if 0

DEFINE_BLOCK(b_continue_mu)
{
    obj_t *params = pair_car(VALUE);
    obj_t *body = pair_cdr(VALUE);
    RETURN(make_procedure(body, params, F_ENV));
}

/*
 * mu (the letter after lambda) is an experimental thing.  Like
 * lambda, mu defines a procedure.  But the mu form is passed to an
 * expansion function before creating the procedure.  The expansion
 * function is read from the file mu-expand.scm.
 *
 * This demonstrates how to call Scheme from C and also how to
 * hook macro processing into the interpreter.
 */

STATIC_ROOT(mu_expand);

#define MU_THEN_GOTO(value, callee, target)				\
    do {								\
	FRAME = MAKE_GOTO target;					\
	FRAME = MAKE_CALL callee;					\
	POP_FUNCTION_ROOTS();						\
	return (value);							\
    } while (0)

DEFINE_SPECIAL_FORM(L"mu")		/* letter after lambda */
{
    /* with lock */ {
	if (!mu_expand) {
	    FILE *fin = fopen("mu-expand.scm", "r");
	    assert(fin);
	    instream_t *ins = make_file_instream(fin);
	    obj_t *form;
	    bool ok = read_stream(ins, &form);
	    assert(ok);
	    assert(pair_car(form) == make_symbol_from_C_str(L"lambda"));
	    obj_t *formals = pair_cadr(form);
	    obj_t *body = pair_cddr(form);
	    obj_t *lib = library_env(r6rs_library());
	    mu_expand = make_special_form_procedure(body, formals, lib);
	}
    }
    AUTO_ROOT(args, make_pair(F_SUBJ, NIL));
    AUTO_ROOT(mu, make_symbol_from_C_str(L"mu"));
    args = make_pair(mu, args);
    MU_THEN_GOTO(mu_expand,
		 (b_accum_operator, args, F_ENV),
		 (b_continue_mu, NIL, F_ENV));
}

#endif
