#ifndef PROC_INCLUDED
#define PROC_INCLUDED

#include "lib.h"
#include "types.h"

/*
 * Macros to define procedures, special forms, and blocks.
 * Procedures and special forms have Scheme bindings.
 * Blocks are special forms with no bindings; they are only callable from C,
 * usually through the GOTO macros below.
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
 * Procedures, blocks, and special forms all have the same calling
 * convention.  They're called with one arg, an eval_frame_t pointer
 * named FRAME, and they return an eval_frame_t pointer for the
 * next frame to execute.
 *
 * The procedure can access FRAME directly or it can access the
 * registers through the F_* macros.
 *
 *   F_PARENT	the frame's parent (caller)
 *   F_CONT	the frame's continuation (next block)
 *   F_SUBJ	the frame's subject.  For procedures and special forms,
 *		this is the list of actual arguments.
 *   F_ENV	the current environment
 *
 * The following registers are only in long-format frames, which
 * are used when accumulating a procedure's arguments.
 *
 *   F_PROC	the procedure whose arguments are being accumulated
 *   F_ARGL	the head of the arg list
 *   F_LARG	the last element of the arg list.
 *
 * Procedures, blocks, and special forms should use these macros to
 * pass control.  A GOTO is a transfer to another function.  The current
 * function is not resumed.  A CALL calls another function, but a call
 * is always followed by a GOTO, so the current function is not
 * resumed.
 *
 * RETURN(val)		return the value to the caller
 *
 * GOTO(target, ...)	transfer to the target function, passing the
 *			specified args.  The args are packed into the
 *			target's FRAME by its frame maker.
 *
 * GOTO_FRAME(make_frame, target, ...)
 * 			transfer to the target function.  The
 *			specified frame maker is used to build the
 *			target's frame.
 *
 * CALL_THEN_GOTO(callee, target)
 *			call the callee, then goto the target.
 *			callee and target are each a tuple of
 *			function name and args.  The args are
 *			passed to the function's frame maker.
 *
 * EVAL_THEN_GOTO(exp, env, target, ...)
 *			a special case of CALL_THEN_GOTO calls
 *			b_eval to evaluate the given expression
 *			in the given environment.
 *
 * TAIL_EVAL(exp, env)	returns the result of evaluating exp, env
 *			to this frame's caller.
 *
 * RAISE(condition)	raise an exception.  TBD.
 */

/* abbreviations */
#define DEFINE_PROC         DEFINE_ANONYMOUS_PROC
#define DEFINE_SPECIAL_FORM DEFINE_ANONYMOUS_SPECIAL_FORM
#define DEFINE_BLOCK        DEFINE_STATIC_BLOCK
#define DECLARE_BLOCK       DECLARE_STATIC_BLOCK

#define DEFINE_EXTERN_PROC(C_name, scheme_name) 			\
    DEFINE_GENERAL_PROC_(r6rs_base_library(), 				\
			 extern, 					\
			 C_name, 					\
			 scheme_name, 					\
			 bind_proc)

#define DEFINE_STATIC_PROC(C_name, scheme_name) 			\
    DEFINE_GENERAL_PROC_(r6rs_base_library(), 				\
			 static, 					\
			 C_name, 					\
			 scheme_name, 					\
			 bind_proc)

#define DEFINE_ANONYMOUS_PROC(scheme_name) 				\
    DEFINE_GENERAL_PROC_(r6rs_base_library(), 				\
                         static, 					\
                         CAT_(anonymous_, __LINE__), 			\
                         scheme_name, 					\
			 bind_proc)

#define DEFINE_EXTERN_SPECIAL_FORM(C_name, scheme_name) 		\
    DEFINE_GENERAL_PROC_(r6rs_base_library(), 				\
			 extern, 					\
			 C_name, 					\
			 scheme_name, 					\
			 bind_special_form)

#define DEFINE_STATIC_SPECIAL_FORM(C_name, scheme_name) 		\
    DEFINE_GENERAL_PROC_(r6rs_base_library(), 				\
		         static, 					\
       			 C_name, 					\
       			 scheme_name, 					\
       			 bind_special_form)

#define DEFINE_ANONYMOUS_SPECIAL_FORM(scheme_name) 			\
    DEFINE_GENERAL_PROC_(r6rs_base_library(), 				\
                         static, 					\
                         CAT_(anonymous_, __LINE__), 			\
                         scheme_name, 					\
       			 bind_special_form)

#define DEFINE_EXTERN_BLOCK(C_name) DECLARE_PROC_(extern, C_name)
#define DEFINE_STATIC_BLOCK(C_name) DECLARE_PROC_(static, C_name)
#define DECLARE_EXTERN_BLOCK(C_name) DECLARE_PROC_(extern, C_name);
#define DECLARE_STATIC_BLOCK(C_name) DECLARE_PROC_(static, C_name);

#define DECLARE_PROC_(storage_class, C_name) \
    storage_class eval_frame_t *C_name(eval_frame_t *FRAME)

#define DEFINE_GENERAL_PROC_(library, 					\
			     storage_class, 				\
			     C_name, 					\
			     scheme_name, 				\
			     binder) 					\
    DECLARE_PROC_(storage_class, C_name); 				\
    __attribute__((constructor)) 					\
    static void CAT_(bind_proc_, __LINE__)(void) 			\
    { 									\
        binder(C_name, library, scheme_name); 				\
    } 									\
    DECLARE_PROC_(storage_class, C_name)

/* concatenate into one identifier */
#define CAT_(a, b) CAT__(a, b)
#define CAT__(a, b) a ## b

/* eval_frame_t member accessors */
#define F_PARENT        ((eval_frame_t *)FRAME->ef_parent)
#define F_CONT          ((C_procedure_t *)FRAME->ef_continuation)
#define F_VAL           ((obj_t *)FRAME->ef_value)
#define F_SUBJ           ((obj_t *)FRAME->ef_subject)
#define F_EXP F_SUBJ
#define F_ENV           ((obj_t *)FRAME->ef_environment)
#define F_PROC          ((obj_t *)FRAME->ef_procedure)
#define F_ARGL          ((obj_t *)FRAME->ef_arglist)
#define F_LARG          ((obj_t *)FRAME->ef_last_arg)

#define RETURN(val) 							\
    do { 								\
	FRAME->ef_parent->ef_value = (val); 				\
	return FRAME->ef_parent; 					\
    } while (0)

/* Evaluate the expression in the environment, then go to the
   target block.
 */
#define EVAL_THEN_GOTO(exp, env, target, ...) \
    CALL_THEN_GOTO((b_eval, (exp), (env)), (target, __VA_ARGS__))

/* Evaluate the expression in the environment and return the
   result to this block's caller.
 */
#define TAIL_EVAL(exp, env) GOTO(b_eval, (exp), (env))

#define RAISE(condition) (assert(false && "XXX implement RAISE"))

/* Direct goto.  Returns from current block, then calls target block
   with specified args.
 */
#define GOTO(target, ...) \
    GOTO_FRAME(TARGET_FRAME_MAKER_(target), target, __VA_ARGS__)

/* Goto with explicit make_frame function.  */
#define GOTO_FRAME(make_frame, target, ...) \
    return make_frame(F_PARENT, target, __VA_ARGS__)

/* Return from this block, then call callee, then goto target.
   Callee and target are tuples (block, arg, arg...).
 */
#define CALL_THEN_GOTO(callee, target) 					\
    do { 								\
	FRAME = MAKE_GOTO_FRAME target; 				\
	FRAME = MAKE_CALL_FRAME callee; 				\
	return FRAME; 							\
    } while (0)

/* Make an activation frame whose continuation is this frame's
   continuation.
 */
#define MAKE_GOTO_FRAME(target, arg1, ...) \
    TARGET_FRAME_MAKER_(target)(F_PARENT, target, (arg1), __VA_ARGS__)

/* Make an activation frame whose continuation is the current frame.
 */
#define MAKE_CALL_FRAME(target, ...) \
    TARGET_FRAME_MAKER_(target)(FRAME, target, __VA_ARGS__)

#define TARGET_FRAME_MAKER_(target) MAKE_##target##_FRAME_

DECLARE_EXTERN_BLOCK(b_eval);
#define MAKE_b_eval_FRAME_	     make_short_frame

struct eval_frame {
    eval_frame_t  *ef_parent;
    C_procedure_t *ef_continuation;
    obj_t         *ef_value;
    obj_t         *ef_subject;
    env_t         *ef_environment;
    obj_t         *ef_procedure;
    obj_t         *ef_arglist;
    obj_t         *ef_last_arg;
};

extern void bind_proc(C_procedure_t, lib_t *library, const char *name);
extern void bind_special_form(C_procedure_t, lib_t *library, const char *name);

extern eval_frame_t *make_short_frame(eval_frame_t *parent,
				      C_procedure_t *continuation,
				      obj_t *subject,
				      env_t *environment);

#endif /* !PROC_INCLUDED */
