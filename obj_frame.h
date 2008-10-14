#ifndef OBJ_FRAME_INCLUDED
#define OBJ_FRAME_INCLUDED

#include "obj_procedure.h"

extern obj_t         *make_short_frame(obj_t         *parent,
				       C_procedure_t *continuation,
				       obj_t         *subject,
				       obj_t         *environment);

extern obj_t         *make_long_frame(obj_t          *parent,
				      C_procedure_t  *continuation,
				      obj_t          *subject,
				      obj_t          *environment,
				      obj_t          *procedure,
				      obj_t          *arg_list,
				      obj_t          *last_arg);

extern bool           is_frame              (obj_t *);
extern bool           is_long_frame         (obj_t *);

extern obj_t         *frame_get_parent      (obj_t *frame);
extern C_procedure_t *frame_get_continuation(obj_t *frame);
extern obj_t         *frame_get_value       (obj_t *frame);
extern obj_t         *frame_get_subject     (obj_t *frame);
extern obj_t         *frame_get_environment (obj_t *frame);

/* These are for long frames only. */
extern obj_t         *frame_get_procedure   (obj_t *frame);
extern obj_t         *frame_get_arg_list    (obj_t *frame);
extern obj_t         *frame_get_last_arg    (obj_t *frame);

#endif /* !OBJ_FRAME_INCLUDED */
