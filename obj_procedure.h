#ifndef OBJ_PROCEDURE_INCLUDED
#define OBJ_PROCEDURE_INCLUDED

#include "obj.h"

// XXX move this struct definition to proc.h.
typedef struct eval_frame {
    obj_t       *ef_frame;
    obj_t       *ef_value;
} eval_frame_t;

typedef eval_frame_t C_procedure_t(eval_frame_t);

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
extern bool   is_procedure(obj_t *);
extern bool   procedure_is_C(obj_t *);
extern bool   procedure_is_special_form(obj_t *);
extern obj_t *procedure_body(obj_t *);
extern obj_t *procedure_args(obj_t *);
extern obj_t *procedure_env(obj_t *);

#endif /* !OBJ_PROCEDURE_INCLUDED */
