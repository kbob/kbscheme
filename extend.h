#ifndef EXTEND_INCLUDED
#define EXTEND_INCLUDED

#include "obj.h"
#include "lib.h"

#define CAT__(a, b) a ## b
#define CAT_(a, b) CAT__(a, b)

#define DEFINE_LIBRARY_PROC(library, name) \
    static obj_t *CAT_(proc_, __LINE__) (obj_t *, env_t *);	\
    __attribute__((constructor)) \
    static void CAT_(register_proc_, __LINE__)(void) \
    { \
        register_proc(CAT_(proc_, __LINE__), library, name); \
    } \
    static obj_t *CAT_(proc_, __LINE__) (obj_t *ARGLIST, env_t *ENV)

#define DEFINE_LIBRARY_SPECIAL_FORM(library, name) \
    static obj_t *CAT_(special_form_, __LINE__) (obj_t *, env_t *);	\
    __attribute__((constructor)) \
    static void CAT_(register_special_form_, __LINE__)(void) \
    { \
        register_special_form(CAT_(special_form_, __LINE__), library, name); \
    } \
    static obj_t *CAT_(special_form_, __LINE__) (obj_t *ARGLIST, env_t *ENV)

#define oldDEFINE_PROC(name) \
    DEFINE_LIBRARY_PROC(r6rs_base_library(), name)

#define oldDEFINE_SPECIAL_FORM(name) \
    DEFINE_LIBRARY_SPECIAL_FORM(r6rs_base_library(), name)

extern void register_proc(C_procedure_t,
			  lib_t *library,
			  const char *name);

extern void register_special_form(C_special_form_t,
			    lib_t *library,
			    const char *name);

#endif /* !EXTEND_INCLUDED */
