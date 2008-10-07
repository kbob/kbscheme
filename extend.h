#ifndef EXTEND_INCLUDED
#define EXTEND_INCLUDED

/* XXX this file is obsolete. */

#include "obj.h"
#include "lib.h"

/* XXX this file is obsolete. */

#define CAT__(a, b) a ## b
#define CAT_(a, b) CAT__(a, b)

/* XXX this file is obsolete. */

#define oldDEFINE_LIBRARY_PROC(library, name) \
    static obj_t *CAT_(proc_, __LINE__) (obj_t *, env_t *);	\
    __attribute__((constructor)) \
    static void CAT_(register_proc_, __LINE__)(void) \
    { \
        register_proc((C_procedure_t *) CAT_(proc_, __LINE__), library, name); \
    } \
    static obj_t *CAT_(proc_, __LINE__) (obj_t *ARGLIST, env_t *ENV)

#define oldDEFINE_LIBRARY_SPECIAL_FORM(library, name) \
    static obj_t *CAT_(special_form_, __LINE__) (obj_t *, env_t *);	\
    __attribute__((constructor)) \
    static void CAT_(register_special_form_, __LINE__)(void) \
    { \
        register_special_form((C_procedure_t *) CAT_(special_form_, __LINE__), library, name); \
    } \
    static obj_t *CAT_(special_form_, __LINE__) (obj_t *ARGLIST, env_t *ENV)

/* XXX this file is obsolete. */

#define oldDEFINE_PROC(name) \
    oldDEFINE_LIBRARY_PROC(r6rs_base_library(), name)

#define oldDEFINE_SPECIAL_FORM(name) \
    oldDEFINE_LIBRARY_SPECIAL_FORM(r6rs_base_library(), name)

/* XXX this file is obsolete. */

extern void register_proc(C_procedure_t, // XXX obsolete
			  lib_t *library,
			  const char *name);

extern void register_special_form(C_procedure_t, // XXX obsolete
				  lib_t *library,
				  const char *name);

/* XXX this file is obsolete. */

#endif /* !EXTEND_INCLUDED */
