#ifndef PROC_INCLUDED
#define PROC_INCLUDED

#include "concat.h"
#include "lib.h"
#include "obj_frame.h"
#include "roots.h"
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
 * convention.  They're called with one arg, an (obj_t *), which is
 * the value returned by the previous procedure, and they return their
 * own result as an (obj_t *).  They also rely on a frame object
 * referenced by the thread-global variable FRAME.  They pass control
 * by creating a new frame and assigning it to FRAME.
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
 *			transfer to the target function.  The
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

#define DEFINE_EXTERN_PROC(C_name, scheme_name)				\
    DEFINE_GENERAL_PROC_(NIL,						\
			 extern,					\
			 C_name,					\
			 scheme_name,					\
			 bind_proc)

#define DEFINE_STATIC_PROC(C_name, scheme_name)				\
    DEFINE_GENERAL_PROC_(NIL,						\
			 static,					\
			 C_name,					\
			 scheme_name,					\
			 bind_proc)

#define DEFINE_ANONYMOUS_PROC(scheme_name)				\
    DEFINE_GENERAL_PROC_(NIL,						\
                         static,					\
                         CAT_(anonymous_, __LINE__),			\
                         scheme_name,					\
			 bind_proc)

#define DEFINE_EXTERN_SPECIAL_FORM(C_name, scheme_name)			\
    DEFINE_GENERAL_PROC_(NIL,						\
			 extern,					\
			 C_name,					\
			 scheme_name,					\
			 bind_special_form)

#define DEFINE_STATIC_SPECIAL_FORM(C_name, scheme_name)			\
    DEFINE_GENERAL_PROC_(NIL,						\
		         static,					\
			 C_name,					\
			 scheme_name,					\
			 bind_special_form)

#define DEFINE_ANONYMOUS_SPECIAL_FORM(scheme_name)			\
    DEFINE_GENERAL_PROC_(NIL,						\
                         static,					\
                         CAT_(anonymous_, __LINE__),			\
                         scheme_name,					\
			 bind_special_form)

#define DEFINE_EXTERN_BLOCK(C_name) DECLARE_PROC_(extern, C_name)
#define DEFINE_STATIC_BLOCK(C_name) DECLARE_PROC_(static, C_name)
#define DECLARE_EXTERN_BLOCK(C_name) DECLARE_PROC_(extern, C_name);
#define DECLARE_STATIC_BLOCK(C_name) DECLARE_PROC_(static, C_name);

#define DECLARE_PROC_(storage_class, C_name) \
    storage_class obj_t *C_name(obj_t *VALUE)

#define ALIAS_NAME(old_library, old_name, new_library, new_name)	\
    __attribute__((constructor))					\
    static void CAT_(alias_proc_, __LINE__)(void)			\
    {									\
	static alias_descriptor_t desc = {				\
	    old_library,						\
	    old_name,							\
	    new_library,						\
	    new_name,							\
	    NULL							\
	};								\
	register_alias(&desc);						\
    }

#define DEFINE_GENERAL_PROC_(library,					\
			     storage_class,				\
			     C_name,					\
			     scheme_name,				\
			     binder)					\
    DECLARE_PROC_(storage_class, C_name);				\
    __attribute__((constructor))					\
    static void CAT_(bind_proc_, __LINE__)(void)			\
    {									\
	static proc_descriptor_t desc = {				\
	    library,							\
	    C_name,							\
	    scheme_name,						\
	    binder,							\
	    NULL							\
	};								\
	register_proc(&desc);						\
    }									\
    DECLARE_PROC_(storage_class, C_name)

/* FRAME member accessors */
#define F_PARENT        (frame_get_parent(FRAME))
#define F_CONT          (frame_get_continuation(FRAME))
#define F_SUBJ          (frame_get_subject(FRAME))
#define F_ENV           (frame_get_environment(FRAME))
#define F_PROC          (frame_get_procedure(FRAME))
#define F_ARGL          (frame_get_arg_list(FRAME))
#define F_LARG          (frame_get_last_arg(FRAME))

/* Return the value as the result of this function. */
#define RETURN(val)							\
    do {								\
	obj_t *val__ = (val);						\
	FRAME = F_PARENT;						\
	POP_FUNCTION_ROOTS();						\
	return val__;							\
    } while (0)

/* Evaluate the expression in the environment and return the
   result to this block's caller.
 */
#define TAIL_EVAL(exp) GOTO(b_eval, (exp), F_ENV)

/* Raise an exception. */
#define RAISE(condition) (assert(false && "XXX implement RAISE"))

/* Evaluate the expression in the environment, then go to the
 * target block.
 */
#define EVAL_THEN_GOTO(exp, env, target, ...) \
    EVAL_THEN_GOTO_FRAME((exp), (env), make_short_frame, (target), __VA_ARGS__)

#define EVAL_THEN_GOTO_FRAME(exp, env, factory, target, ...)		\
    CALL_THEN_GOTO_FRAME((b_eval, (exp), (env)),			\
                         ((factory), (target), __VA_ARGS__))

/* Direct goto.  Returns from current block, then calls target block
   with specified args.
 */
#define GOTO(target, ...) \
    GOTO_FRAME(make_short_frame, (target), __VA_ARGS__)

/* Goto with explicit frame factory.  */
#define GOTO_FRAME(factory, target, ...)				\
    do {								\
        FRAME = MAKE_GOTO_FRAME((factory), (target), __VA_ARGS__);	\
        POP_FUNCTION_ROOTS();						\
	return NIL;							\
    } while (0)

/* Return from this block, then call callee, then goto target.
 * Callee and target are tuples (block, arg, arg...).
 */
#define CALL_THEN_GOTO(callee, target)					\
    do {								\
	FRAME = MAKE_GOTO target;					\
	FRAME = MAKE_CALL callee;					\
	POP_FUNCTION_ROOTS();						\
	return NIL;							\
    } while (0)

#define CALL_THEN_GOTO_FRAME(callee, target)				\
    do {								\
	FRAME = MAKE_GOTO_FRAME target;					\
	FRAME = MAKE_CALL callee;					\
	POP_FUNCTION_ROOTS();						\
	return NIL;							\
    } while (0)

/* Make an activation frame whose continuation is this frame's
   continuation.
 */
#define MAKE_GOTO(target, ...) \
    MAKE_GOTO_FRAME(make_short_frame, (target), __VA_ARGS__)

#define MAKE_GOTO_FRAME(factory, target, ...) \
    (factory(F_PARENT, (target), __VA_ARGS__))

/* Make an activation frame whose continuation is the current frame.
 */
#define MAKE_CALL(target, ...) (make_short_frame(FRAME, (target), __VA_ARGS__))

typedef void binder_t(C_procedure_t, lib_t *, const wchar_t *name);
typedef struct proc_descriptor proc_descriptor_t;
typedef struct alias_descriptor alias_descriptor_t;

struct proc_descriptor {
    lib_t              *pd_library;
    C_procedure_t      *pd_proc;
    const wchar_t      *pd_name;
    binder_t           *pd_binder;
    proc_descriptor_t  *pd_next;
};

struct alias_descriptor {
    lib_t              *ad_old_library;
    const wchar_t      *ad_old_name;
    lib_t              *ad_new_library;
    const wchar_t      *ad_new_name;
    alias_descriptor_t *ad_next;
};

extern obj_t *FRAME;

DECLARE_EXTERN_BLOCK(b_eval);
DECLARE_EXTERN_BLOCK(b_accum_operator);

extern obj_t *eval_application(obj_t *proc, obj_t *args);

extern void register_proc(proc_descriptor_t *desc);
extern void register_procs(void);
extern void register_alias(alias_descriptor_t *);

extern void bind_proc(C_procedure_t, lib_t *library, const wchar_t *name);
extern void bind_special_form(C_procedure_t,
			      lib_t *library,
			      const wchar_t *name);

#endif /* !PROC_INCLUDED */
