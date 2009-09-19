#ifndef OBJ_PROCEDURE_INCLUDED
#define OBJ_PROCEDURE_INCLUDED

#include "obj.h"

typedef obj_t *C_procedure_t(obj_t *value);

extern obj_t *make_procedure(obj_t *code, obj_t *arglist, obj_t *env);
extern obj_t *make_C_procedure(C_procedure_t *code,
			       obj_t *arglist,
			       obj_t *env);
extern obj_t *make_special_form_procedure(obj_t *code,
					  obj_t *arglist,
					  obj_t *env);
extern obj_t *make_C_special_form_procedure(C_procedure_t *code,
					    obj_t *arglist,
					    obj_t *env);
extern obj_t *make_xformer_proc(obj_t *code,
				obj_t *arglist,
				obj_t *env);
extern obj_t *make_C_xformer_proc(C_procedure_t *code,
				  obj_t *arglist,
				  obj_t *env);

extern bool   is_procedure(obj_t *);
extern bool   procedure_is_C(obj_t *);
extern bool   procedure_is_xformer(obj_t *);
extern bool   procedure_is_special_form(obj_t *);
extern obj_t *procedure_body(obj_t *);
extern obj_t *procedure_args(obj_t *);
extern obj_t *procedure_env(obj_t *);

#endif /* !OBJ_PROCEDURE_INCLUDED */
