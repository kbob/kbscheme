#ifndef MEM_FIXVEC_INCLUDED
#define MEM_FIXVEC_INCLUDED

#include "mem.h"

#if !OLD_MEM

/*
 * A fixvec is an abstract base type for fixed-length vectors of
 * object pointers.
 * 
 * Example fixvecs: symbol procedure.
 */

extern void mem_fixvec_create_ops(mem_ops_t *,
				  wchar_t *name,
				  size_t len,
				  mem_init_op,
				  mem_free_op);
extern obj_t *alloc_fixvec1(mem_ops_t *, obj_t *);
extern obj_t *alloc_fixvec2(mem_ops_t *, obj_t *, obj_t *);
extern obj_t *alloc_fixvec3(mem_ops_t *, obj_t *, obj_t *, obj_t *);
extern obj_t *alloc_fixvec4(mem_ops_t *, obj_t *, obj_t *, obj_t *, obj_t *);

#endif /* !OLD_MEM */

#endif /* !MEM_FIXVEC_INCLUDED */
