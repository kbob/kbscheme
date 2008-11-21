#include "obj_frame.h"

#include <assert.h>

#include "mem_mixvec.h"

static mem_ops_t short_frame_ops;
static mem_ops_t long_frame_ops;

#define I_CONT   0
#define P_PARENT 0
#define P_SUBJ   1
#define P_ENV    2
#define P_PROC   3
#define P_ARGL   4
#define P_LARG   5

obj_t *make_short_frame(obj_t         *parent,
			C_procedure_t *continuation,
			obj_t         *subject,
			obj_t         *environment)
{
    assert_in_tospace(parent);
    assert_in_tospace(subject);
    assert_in_tospace(environment);
    PUSH_ROOT(parent);
    PUSH_ROOT(subject);
    PUSH_ROOT(environment);
    if (!short_frame_ops.mo_super)
	mem_mixvec_create_ops(&short_frame_ops, L"short frame", 1, 3);
    obj_t *frame = alloc_mixvec_1_3(&short_frame_ops);
    mixvec_1_3_set_int(frame, I_CONT,   (intptr_t)continuation);
    mixvec_1_3_set_ptr(frame, P_PARENT, parent);
    mixvec_1_3_set_ptr(frame, P_SUBJ,   subject);
    mixvec_1_3_set_ptr(frame, P_ENV,    environment);
    POP_FUNCTION_ROOTS();
    return frame;
}

obj_t *make_long_frame(obj_t          *parent,
		       C_procedure_t  *continuation,
		       obj_t          *subject,
		       obj_t          *environment,
		       obj_t          *procedure,
		       obj_t          *arg_list,
		       obj_t          *last_arg)
{
    assert_in_tospace(parent);
    assert_in_tospace(subject);
    assert_in_tospace(environment);
    assert_in_tospace(procedure);
    assert_in_tospace(arg_list);
    assert_in_tospace(last_arg);
    PUSH_ROOT(parent);
    PUSH_ROOT(subject);
    PUSH_ROOT(environment);
    PUSH_ROOT(procedure);
    PUSH_ROOT(arg_list);
    PUSH_ROOT(last_arg);
    if (!long_frame_ops.mo_super)
	mem_mixvec_create_ops(&long_frame_ops, L"long frame", 1, 6);
    obj_t *frame = alloc_mixvec_1_6(&long_frame_ops);
    mixvec_1_6_set_int(frame, I_CONT,   (intptr_t)continuation);
    mixvec_1_6_set_ptr(frame, P_PARENT, parent);
    mixvec_1_6_set_ptr(frame, P_SUBJ,   subject);
    mixvec_1_6_set_ptr(frame, P_ENV,    environment);
    mixvec_1_6_set_ptr(frame, P_PROC,   procedure);
    mixvec_1_6_set_ptr(frame, P_ARGL,   arg_list);
    mixvec_1_6_set_ptr(frame, P_LARG,   last_arg);
    POP_FUNCTION_ROOTS();
    return frame;
}

bool is_frame(obj_t *obj)
{
    assert_in_tospace(obj);
    if (is_null(obj))
	return false;
    mem_ops_t *ops = OBJ_MEM_OPS(obj);
    return ops == &short_frame_ops || ops == &long_frame_ops;
}

bool is_long_frame(obj_t *obj)
{
    assert_in_tospace(obj);
    return obj && OBJ_MEM_OPS(obj) == &long_frame_ops;
}

obj_t *frame_get_parent(obj_t *frame)
{
    assert_in_tospace(frame);
    assert(is_frame(frame));
    return mixvec_1_6_get_ptr(frame, P_PARENT);
}

C_procedure_t *frame_get_continuation(obj_t *frame)
{
    assert_in_tospace(frame);
    assert(is_frame(frame));
    return (C_procedure_t *)mixvec_1_6_get_int(frame, I_CONT);
}

obj_t *frame_get_subject(obj_t *frame)
{
    assert_in_tospace(frame);
    assert(is_frame(frame));
    return mixvec_1_6_get_ptr(frame, P_SUBJ);
}

obj_t *frame_get_environment(obj_t *frame)
{
    assert_in_tospace(frame);
    assert(is_frame(frame));
    return mixvec_1_6_get_ptr(frame, P_ENV);
}

extern obj_t *frame_get_procedure(obj_t *frame)
{
    assert_in_tospace(frame);
    assert(is_long_frame(frame));
    return mixvec_1_6_get_ptr(frame, P_PROC);
}

extern obj_t *frame_get_arg_list(obj_t *frame)
{
    assert_in_tospace(frame);
    assert(is_long_frame(frame));
    return mixvec_1_6_get_ptr(frame, P_ARGL);
}

extern obj_t *frame_get_last_arg(obj_t *frame)
{
    assert_in_tospace(frame);
    assert(is_long_frame(frame));
    return mixvec_1_6_get_ptr(frame, P_LARG);
}
