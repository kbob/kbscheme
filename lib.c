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

static lib_t *make_library(obj_t *namespec)
{
    PUSH_ROOT(namespec);
    AUTO_ROOT(env, make_env(NIL));
    obj_t *lib = make_pair(env, namespec);
    POP_FUNCTION_ROOTS();
    return lib;
}

env_t *library_env(lib_t *lib)
{
    return pair_car(lib);
}

obj_t *library_namespec(obj_t *lib)
{
    return pair_cdr(lib);
}

static bool lists_are_equiv(obj_t *a, obj_t *b)
{
    while (a && b) {
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
    return !a && !b;
}

obj_t *find_library(const wchar_t *namespec)
{
    instream_t *in = make_string_instream(namespec, wcslen(namespec));
    obj_t *namespec_list;
    bool ok = read_stream(in, &namespec_list);
    assert(ok);
    delete_instream(in);
    obj_t *p;
    for (p = library_list; p; p = pair_cdr(p)) {
	obj_t *lib = pair_car(p);
	if (lists_are_equiv(namespec_list, library_namespec(lib)))
	    return lib;
    }
    AUTO_ROOT(lib, make_library(namespec_list));
    library_list = make_pair(lib, library_list);
    POP_ROOT(lib);
    return lib;
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
	(void) find_library(desc->ld_namespec);
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

static void eval_library_form(obj_t *form)
{
    // verify car is 'library'
    // verify cadr is 'export'
    // save export list.
    // verify cddr is 'import'

    // working env = [empty]
    // process import list.
    //    for each import,
    //        find library
    //        prepend library's export env to working env (as a new frame).
    //        fail on complex import specs.
    // given env, process library body.
    // process export list.
    //     export_env = [empty]
    //     for each export,
    //         immutably bind ext name to int name in export_env
}

static bool load_library(const wchar_t *libname)
{
#if 1
    char filename[PATH_MAX + 1];
    const char **dirp;
    for (dirp = lib_path; *dirp; dirp++) {
	if (snprintf(filename, sizeof filename, "%s/%ls.scm", *dirp, libname) >= sizeof filename)
	    continue;
	printf("filename=\"%s\"\n", filename);
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
#endif
    return false;
}

void load_libraries(void)
{
    register_libraries();
    if (!load_library(STD_LIBRARY))
	assert(false && "No std lib");
}

lib_t *r6rs_library(void)
{
    if (!r6rs_lib) {
	AUTO_ROOT(sym, make_fixnum(6));
	AUTO_ROOT(p, make_pair(sym, NIL));
	p = make_pair(p, NIL);
	sym = make_symbol(L"rnrs");
	p = make_pair(sym, p);
	r6rs_lib = make_library(p);
	library_descriptor_t *desc;
	AUTO_ROOT(env, library_env(r6rs_lib));
	AUTO_ROOT(frame, NIL);
	for (desc = lib_descriptors; desc; desc = desc->ld_next) {
	    obj_t *lib = find_library(desc->ld_namespec);
	    frame = pair_car(library_env(lib));
	    for (; frame; frame = pair_cdr(frame)) {
		obj_t *binding = pair_car(frame);
		env_bind(env, binding_name(binding),
			      binding_type(binding),
			      binding_value(binding));
	    }
	}
	POP_FUNCTION_ROOTS();
    }
    return r6rs_lib;
}
