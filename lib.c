#include "lib.h"

#include <assert.h>
#include <limits.h>
#include <stdio.h>

#include "eval.h"
#include "io.h"
#include "read.h"
#include "roots.h"
#include "types.h"

ROOT(library_list);
ROOT(r6rs_lib);

static library_descriptor_t *lib_descriptors;

/*
 * verbs
 *    make - unconditionally create
 *    lookup - get if existent
 *    find - lookup or make
 *    load - get or read from files.
 */

static obj_t *parse_namespec(const wchar_t *namespec_str)
{
    obj_t *namespec_list;
    instream_t *in = make_string_instream(namespec_str, wcslen(namespec_str));
    bool ok = read_stream(in, &namespec_list);
    assert(ok);
    delete_instream(in);
    return namespec_list;
}

static obj_t *make_library(obj_t *namespec)
{
    PUSH_ROOT(namespec);
    AUTO_ROOT(env, make_env(NIL));
    obj_t *lib = make_pair(env, namespec);
    POP_FUNCTION_ROOTS();
    return lib;
}

env_t *library_env(obj_t *lib)
{
    return pair_car(lib);
}

obj_t *library_namespec(obj_t *lib)
{
    return pair_cdr(lib);
}

static bool lists_are_equiv(obj_t *a, obj_t *b)
{
    while (!is_null(a) && !is_null(b)) {
	obj_t *car_a = pair_car(a);
	obj_t *car_b = pair_car(b);
	if (car_a != car_b &&
	    (!is_fixnum(car_a) || !is_fixnum(car_b) ||
	     !fixnum_value(car_a) == fixnum_value(car_b)) &&
	    (!is_pair(car_a) || !is_pair(car_b) ||
	     !lists_are_equiv(car_a, car_b))
	    )
	    return false;
	a = pair_cdr(a);
	b = pair_cdr(b);
    }
    return is_null(a) && is_null(b);
}

static obj_t *lookup_library(obj_t *namespec)
{
    obj_t *p;
    for (p = library_list; !is_null(p); p = pair_cdr(p)) {
	obj_t *lib = pair_car(p);
	if (lists_are_equiv(namespec, library_namespec(lib)))
	    return lib;
    }
    return NIL;
}

obj_t *find_library(obj_t *namespec_list)
{
    obj_t *lib = lookup_library(namespec_list);
    if (is_null(lib)) {
	lib = make_library(namespec_list);
	PUSH_ROOT(lib);
	library_list = make_pair(lib, library_list);
	POP_ROOT(lib);
    }
    return lib;
}

obj_t *find_library_str(const wchar_t *namespec_str)
{
    obj_t *namespec_list = parse_namespec(namespec_str);
    return find_library(namespec_list);
}

void register_C_library(library_descriptor_t *desc)
{
    desc->ld_next = lib_descriptors;
    lib_descriptors = desc;
}

void register_libraries(void)
{
    library_descriptor_t *desc;
    for (desc = lib_descriptors; desc; desc = desc->ld_next)
	/* builds library list as a side-effect. */
	(void) find_library_str(desc->ld_namespec);
}

static const char *lib_path[] = {
    ".",
    "lib",
    NULL,
    NULL
};

void set_exec_path(const char *exec_path)
{
    lib_path[2] = exec_path;
}

static const wchar_t STD_LIBRARY[] = L"r6rs";

bool is_valid_library_form(obj_t *form)
{
    /* verify car is 'library'
     * verify caaddr is 'export'
     * verify caadddr is 'import'
     */
    PUSH_ROOT(form);
    AUTO_ROOT(library_sym, make_symbol_from_C_str(L"library"));
    AUTO_ROOT(export_sym,  make_symbol_from_C_str(L"export"));
    AUTO_ROOT(import_sym,  make_symbol_from_C_str(L"import"));
    POP_FUNCTION_ROOTS();
    if (!is_pair(form))
	return false;
    if (pair_car(form) != library_sym)
	return false;
    obj_t *cdr = pair_cdr(form);
    if (!is_pair(cdr))
	return false;
    obj_t *cddr = pair_cdr(cdr);
    obj_t *caddr = pair_car(cddr);
    if (!is_pair(caddr))
	return false;
    if (pair_car(caddr) != export_sym)
	return false;
    obj_t *cdddr = pair_cdr(cddr);
    obj_t *cadddr = pair_car(cdddr);
    if (pair_car(cadddr) != import_sym)
	return false;
    return true;
}

static void eval_library_form(obj_t *form)
{
    PUSH_ROOT(form);
    bool ok = is_valid_library_form(form);
    assert(ok);

    AUTO_ROOT(working_env, NIL);
    AUTO_ROOT(import_list, pair_cdadddr(form));
    while (!is_null(import_list)) {
	/*
         * Not implementing full import matching - import spec must
         * exactly match library name, and full namespace is imported.
         */
	obj_t *namespec = pair_car(import_list);
	obj_t *lib = lookup_library(namespec);
	assert(lib);
	working_env = join_envs(library_env(lib), working_env);
	import_list = pair_cdr(import_list);
    }
    POP_ROOT(import_list);
    AUTO_ROOT(body, pair_cddddr(form));
    while (!is_null(body)) {
	(void) eval(pair_car(body), working_env);
	body = pair_cdr(body);
    }
    POP_ROOT(body);
    obj_t *namespec = pair_cadr(form);
    AUTO_ROOT(new_lib, find_library(namespec));
    AUTO_ROOT(export_list, pair_cdaddr(form));
    while (export_list) {
	obj_t *name = pair_car(export_list);
	obj_t *binding = env_lookup(working_env, name);
	obj_t *value = binding_value(binding);
	env_bind(library_env(new_lib), name, BT_LEXICAL, M_IMMUTABLE, value);
	export_list = pair_cdr(export_list);
    }
    POP_FUNCTION_ROOTS();
}

static bool load_library(const wchar_t *libname)
{
    char filename[PATH_MAX + 1];
    const char **dirp;
    for (dirp = lib_path; *dirp; dirp++) {
	if (snprintf(filename, sizeof filename,
		     "%s/%ls.scm", *dirp, libname) >= sizeof filename)
	    continue;
	FILE *f = fopen(filename, "r");
	if (f == NULL)
	    continue;
	instream_t *in = make_file_instream(f);
	obj_t *form;
	while (read_stream(in, &form))
	    eval_library_form(form);
	delete_instream(in);
	fclose(f);
	return true;
    }
    return false;
}

void load_libraries(void)
{
    if (!load_library(STD_LIBRARY))
	assert(false && "No std lib");
}

obj_t *r6rs_library(void)
{
    if (!r6rs_lib)
	r6rs_lib = find_library_str(L"(rnrs (6))");
    return r6rs_lib;
}
