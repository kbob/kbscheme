#ifndef LIB_INCLUDED
#define LIB_INCLUDED

#include "env.h"
#include "obj.h"

#define LIBRARY(namespec)						\
    static library_descriptor_t current_library_ = {			\
	namespec,							\
	NULL								\
    };									\
									\
    __attribute__((constructor))					\
    static void UNIQ_IDENT(register_library_)(void)			\
    {									\
	register_C_library(&current_library_);				\
    }

typedef obj_t lib_t;

typedef struct library_descriptor library_descriptor_t;

struct library_descriptor {
    const wchar_t        *ld_namespec;
    library_descriptor_t *ld_next;
};

extern obj_t *find_library(const wchar_t *namespec);
extern env_t *library_env(lib_t *);
extern env_t *library_namespec(lib_t *);

extern void register_C_library(library_descriptor_t *);
extern void register_libraries(void);

extern lib_t *r6rs_library(void);

#endif /* !LIB_INCLUDED */
