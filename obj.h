/*
 * 
 */

#ifndef MEM_INCLUDED
#define MEM_INCLUDED

#include <stddef.h>			/* for wchar_t */

#include "bool.h"

typedef struct object obj_t;		/* defined in mem.c */

/* null methods */
extern obj_t      *make_null(void);
extern bool        is_null(obj_t *);

/* boolean methods */
extern obj_t      *make_boolean(bool value);
extern bool        is_boolean(obj_t *);
extern bool        boolean_value(obj_t *);

/* fixnum methods */
extern obj_t      *make_fixnum(int value);
extern bool        is_fixnum(obj_t *);
extern int         fixnum_value(obj_t *);

/* character methods */
extern obj_t      *make_character(wchar_t value);
extern bool        is_character(obj_t *);
extern wchar_t     character_value(obj_t *);

/* string methods */
extern obj_t      *make_string(wchar_t *value);
extern bool        is_string(obj_t *);
extern wchar_t    *string_value(obj_t *);

/* symbol methods */
extern obj_t      *make_symbol(wchar_t *name);
extern bool        is_symbol(obj_t *);
extern obj_t      *symbol_name(obj_t *);

/* pair methods */
extern obj_t      *make_pair(obj_t *car, obj_t *cdr);
extern bool        is_pair(obj_t *);
extern obj_t      *pair_car(obj_t *);
extern obj_t      *pair_cdr(obj_t *);
extern void        pair_set_car(obj_t *pair, obj_t *car);
extern void        pair_set_cdr(obj_t *pair, obj_t *cdr);

/* procedure methods */
#define NEW_EVAL 1

#if NEW_EVAL
typedef struct continuation continuation_t;
typedef enum prim_state prim_state_t;
typedef prim_state_t C_procedure_t(continuation_t *);
typedef prim_state_t C_special_form_t(continuation_t *);
#else
typedef obj_t     *C_procedure_t(obj_t *arglist, obj_t *env);
typedef obj_t     *C_special_form_t(obj_t *arglist, obj_t *env);
#endif
extern obj_t      *make_procedure(obj_t *code, obj_t *arglist, obj_t *env);
extern obj_t      *make_C_procedure(C_procedure_t *code,
				    obj_t *arglist,
				    obj_t *env);
extern obj_t      *make_special_form_procedure(obj_t *code,
					 obj_t *arglist,
					 obj_t *env);
extern obj_t      *make_C_special_form_procedure(C_special_form_t *code,
						 obj_t *arglist,
						 obj_t *env);
extern bool        is_procedure(obj_t *);
extern bool        procedure_is_C(obj_t *);
extern bool        procedure_is_special_form(obj_t *);
extern obj_t      *procedure_code(obj_t *);
extern obj_t      *procedure_args(obj_t *);
extern obj_t      *procedure_env(obj_t *);

#endif /* !MEM_INCLUDED */
