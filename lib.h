#ifndef LIB_INCLUDED
#define LIB_INCLUDED

#include "env.h"
#include "obj.h"
#include "uniq.h"

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

typedef struct library_descriptor library_descriptor_t;

struct library_descriptor {
    const wchar_t        *ld_namespec;
    library_descriptor_t *ld_next;
};

extern obj_t *find_library_str(const wchar_t *namespec);
extern env_t *library_env(obj_t *);
extern env_t *library_namespec(obj_t *);

extern void register_C_library(library_descriptor_t *);
extern void register_libraries(void);
extern void load_libraries(void);

extern void set_exec_path(const char *exec_path);
extern obj_t *r6rs_library(void);

#endif /* !LIB_INCLUDED */
