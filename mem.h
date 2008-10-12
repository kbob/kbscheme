#ifndef MEM_INCLUDED
#define MEM_INCLUDED

#include <wchar.h>

#include "bool.h"
#include "obj.h"

#define ROOT STATIC_ROOT

#define STATIC_ROOT(name) GENERAL_ROOT_(static, name)

#define GENERAL_ROOT_(storage_class, name) \
    storage_class obj_t *name; \
    __attribute__((constructor)) \
    static void mem_record_root_ ## name(void) \
    { \
        mem_record_root(&name, L ## #name); \
    } \

typedef struct mem_ops mem_ops_t;

struct mem_ops {

    /* name of this object class */
    const wchar_t *mo_name;

    /* superclass pointer */
    mem_ops_t *mo_super;

    /* initialize a newly allocated object. */
    void (*mo_init)(const obj_t *, size_t);

    /* get the object's size in bytes. */
    size_t (*mo_size)(const obj_t *);

    /* get the object's i'th heap pointer. */
    obj_t *(*mo_get_ptr)(obj_t *, size_t index);

    /* set the object's i'th heap pointer. */
    void (*mo_set_ptr)(obj_t *, size_t index, obj_t *);

    /* set the GC mark. */
    bool (*mo_get_mark)(const obj_t *);

    /* get the GC mark. */
    void (*mo_set_mark)(obj_t *, bool);

    /* "finalize" (free) the object. */
    void (*mo_free)(obj_t *);

};

extern void mem_init_heap(size_t usable_size);

extern obj_t *mem_alloc_obj(const mem_ops_t *, size_t);

extern void mem_record_root(obj_t **root, wchar_t *name);

#endif /* !MEM_INCLUDED */

// define mem_fixvec
// define mem_bytearray
// define mem_pair

// def gc():
//     
