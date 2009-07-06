#ifndef EXPAND_INCLUDED
#define EXPAND_INCLUDED

#include "proc.h"

extern obj_t *expand(obj_t *form);
DECLARE_EXTERN_BLOCK(begin)
DECLARE_EXTERN_BLOCK(define)
DECLARE_EXTERN_BLOCK(define_syntax)
DECLARE_EXTERN_BLOCK(let_syntax)
DECLARE_EXTERN_BLOCK(letrec_syntax)

#endif /* !EXPAND_INCLUDED */
