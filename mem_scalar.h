#ifndef MEM_SCALAR_INCLUDED
#define MEM_SCALAR_INCLUDED

#include "mem.h"

#if !OLD_MEM

/*
 * A scalar is an abstract base type for data elements that do not
 * contain object pointers.
 * 
 * Example scalars: fixnum, flonum, character.
 */

extern void mem_scalar_create_ops(mem_ops_t *,
				  wchar_t *name,
				  mem_init_op,
				  mem_free_op,
				  mem_size_op);

#endif

#endif /* !MEM_SCALAR_INCLUDED */
