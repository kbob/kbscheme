#ifndef PROC_INCLUDED
#define PROC_INCLUDED

#define OLD_EVAL 0

/*
 * Macros to define procedures, special forms, and blocks.
 * Procedures and special forms have Scheme bindings.
 * Blocks are special forms with no bindings; they are only callable from C.
 *
 * DEFINE_PROC(scheme_name)
 *        short for DEFINE_ANONYMOUS_PROC.
 *
 * DEFINE_SPECIAL_FORM(scheme_name)
 *        short for DEFINE_ANONYMOUS_SPECIAL_FORM.
 *
 * DEFINE_BLOCK(C_name)
 *	  short for DEFINE_STATIC_BLOCK.
 *
 * DECLARE_BLOCK(C_name)
 *	  short for DECLARE_STATIC_BLOCK.
 *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 *
 * DEFINE_EXTERN_PROC(C_name, scheme_name)
 *        Define a procedure with extern scope and the given names in
 *        C and in Scheme.  C_name is an identifier, but scheme_name
 *        is a string.
 *
 * DEFINE_STATIC_PROC(C_name, scheme_name)
 *        Define a procedure with file static scope and the given
 *        names in C and in Scheme.  C_name is an identifier, but
 *        scheme_name is a string.
 *
 * DEFINE_ANONYMOUS_PROC(scheme_name)
 *        Define a procedure with the given name in scheme.  Its
 *        name in C is auto-generated.  scheme_name is a string.
 *
 * DEFINE_EXTERN_SPECIAL_FORM(C_name, scheme_name)
 * DEFINE_STATIC_SPECIAL_FORM(C_name, scheme_name)
 * DEFINE_ANONYMOUS_SPECIAL_FORM(scheme_name)
 *        Define a special form with the given names and scope.
 *
 * DEFINE_EXTERN_BLOCK(C_name)
 * DEFINE_STATIC_BLOCK(C_name)
 *        Define a block with the given C-language name and scope.
 *
 * DECLARE_EXTERN_BLOCK(C_name)
 * DECLARE_STATIC_BLOCK(C_name)
 *        Declare a block without defining it.  (In C, a declaration
 *        is a function prototype; a definition is a function
 *        including its body.)
 *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 *
 * Within a procedure (or block or special form), 
 *
 */

#include "lib.h"

/* abbreviations */
#define DEFINE_PROC         DEFINE_ANONYMOUS_PROC
#define DEFINE_SPECIAL_FORM DEFINE_ANONYMOUS_SPECIAL_FORM
#define DEFINE_BLOCK        DEFINE_STATIC_BLOCK
#define DECLARE_BLOCK       DECLARE_STATIC_BLOCK

#define DEFINE_EXTERN_PROC(C_name, scheme_name) \
    DEFINE_GENERAL_PROC_(r6rs_base_library(), \
			 extern, \
			 C_name, \
			 scheme_name, \
			 bind_proc)

#define DEFINE_STATIC_PROC(C_name, scheme_name) \
    DEFINE_GENERAL_PROC_(r6rs_base_library(), \
			 static, \
			 C_name, \
			 scheme_name, \
			 bind_proc)

#define DEFINE_ANONYMOUS_PROC(scheme_name) \
    DEFINE_GENERAL_PROC_(r6rs_base_library(), \
                         static, \
                         CAT_(anonymous_, __LINE__), \
                         scheme_name, \
			 bind_proc)

#define DEFINE_EXTERN_SPECIAL_FORM(C_name, scheme_name) \
    DEFINE_GENERAL_PROC_(r6rs_base_library(), \
			 extern, \
			 C_name, \
			 scheme_name, \
			 bind_special_form)

#define DEFINE_STATIC_SPECIAL_FORM(C_name, scheme_name) \
    DEFINE_GENERAL_PROC_(r6rs_base_library(), \
		         static, \
       			 C_name, \
       			 scheme_name, \
       			 bind_special_form)

#define DEFINE_ANONYMOUS_SPECIAL_FORM(scheme_name) \
    DEFINE_GENERAL_PROC_(r6rs_base_library(), \
                         static, \
                         CAT_(anonymous_, __LINE__), \
                         scheme_name, \
       			 bind_special_form)

#define DEFINE_EXTERN_BLOCK(C_name) DECLARE_PROC_(extern, C_name)
#define DEFINE_STATIC_BLOCK(C_name) DECLARE_PROC_(static, C_name)
#define DECLARE_EXTERN_BLOCK(C_name) DECLARE_PROC_(extern, C_name);
#define DECLARE_STATIC_BLOCK(C_name) DECLARE_PROC_(static, C_name);

#define DECLARE_PROC_(storage_class, C_name) \
    storage_class eval_frame_t *C_name(eval_frame_t *FRAME)

#define DEFINE_GENERAL_PROC_(library, \
			     storage_class, \
			     C_name, \
			     scheme_name, \
			     binder) \
    DECLARE_PROC_(storage_class, C_name); \
    __attribute__((constructor)) \
    static void CAT_(bind_proc_, __LINE__)(void) \
    { \
        binder(C_name, library, scheme_name); \
    } \
    DECLARE_PROC_(storage_class, C_name)

/* concatenate into one identifier */
#define CAT_(a, b) CAT__(a, b)
#define CAT__(a, b) a ## b

/* eval_frame_t member accessors */
#define F_PARENT        (FRAME->ef_parent)
#define F_CONT          (FRAME->ef_continuation)
#define F_VAL           (FRAME->ef_value)
#define F_SUBJ           (FRAME->ef_subject)
#define F_EXP F_SUBJ
#define F_ENV           (FRAME->ef_environment)
#define F_PROC          (FRAME->ef_procedure)
#define F_ARGL          (FRAME->ef_arglist)
#define F_ARGT          (FRAME->ef_last_arg)
#define F_NXA           (FRAME->ef_next_arg)

/* eval_frame_t member setters */
#define F_SET_PARENT(x) (FRAME->ef_parent = (x))
#define F_SET_CONT(x)   (FRAME->ef_continuation = (x))
#define F_SET_VAL(x)    (FRAME->ef_value = (x))
#define F_SET_EXP(x)    (FRAME->ef_subject = (x))
#define F_SET_ENV(x)    (FRAME->ef_environment = (x))
#define F_SET_PROC(x)   (FRAME->ef_procedure = (x))
#define F_SET_ARGL(x)   (FRAME->ef_arglist = (x))
#define F_SET_ARGT(x)   (FRAME->ef_last_arg = (x))
#define F_SET_NXA(x)    (FRAME->ef_next_arg = (x))

#define RETURN(val) \
    do { \
        obj_t *val__ = (val); \
        FRAME = F_PARENT; \
        F_SET_VAL(val__); \
        return FRAME; \
    } while (0)

#define EVAL(exp, env) \
    do { \
        obj_t *exp__ = (exp); \
        obj_t *env__ = (env); \
        FRAME = make_frame(FRAME); \
        F_SET_CONT(b_eval); \
        F_SET_EXP(exp__); \
        F_SET_ENV(env__); \
        return FRAME; \
    } while (0)
#undef XXX_EVAL
#define XXX_EVAL(exp, env) \
    return MAKE_CALL_FRAME(MAKE_EVAL_FRAME, (exp), (env))

#define TAIL_EVAL(exp, env) \
    do { \
        obj_t *exp__ = (exp); \
        obj_t *env__ = (env); \
        FRAME = make_frame(FRAME); \
        FRAME->ef_parent = FRAME->ef_parent->ef_parent->ef_parent; \
        F_SET_CONT(b_eval); \
        F_SET_EXP(exp__); \
        F_SET_ENV(env__); \
        return FRAME; \
    } while (0)

#define RAISE(condition) (assert(false))

struct eval_frame {
    eval_frame_t  *ef_parent;
    C_procedure_t *ef_continuation;
    obj_t         *ef_value;
    obj_t         *ef_subject;
    env_t         *ef_environment;
    obj_t         *ef_procedure;
    obj_t         *ef_arglist;
    obj_t         *ef_last_arg;
    obj_t         *ef_next_arg;		// XXX delete
};

extern void bind_proc(C_procedure_t, lib_t *library, const char *name);
extern void bind_special_form(C_procedure_t, lib_t *library, const char *name);

extern eval_frame_t *make_frame(eval_frame_t *parent);

#endif /* !PROC_INCLUDED */
