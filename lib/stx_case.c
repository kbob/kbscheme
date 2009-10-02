#include "proc.h"
#include "test.h"

#include "obj_syntax.h"

LIBRARY(L"(rnrs syntax-case (6))")

/*
 * r6rs-lib 12.3.  Transformers
 *
 * (make-variable-transformer proc)
 */

/*
 * r6rs-lib 12.4.  Parsing input and producing output
 *
 * (syntax-case <expression> (<literal> ...)	# syntax
 *     <syntax-case clause> ...)
 *
 * (syntax <template>)				# syntax
 */

/*
 * r6rs-lib 12.5.  Identifier predicates
 *
 * (identifier? obj)
 *
 * (bound-identifier=? id1 id2)
 *
 * (free-identifier=? id1 id2)
 */

/*
 * r6rs-lib 12.6.  Syntax-object and datum conversions
 *
 * (syntax->datum syntax-object)		# procedure
 *
 * (datum->syntax template-id datum)		# procedure
 */ 

DEFINE_PROC(L"syntax->datum")
{
    RETURN(syntax_expr(pair_car(F_SUBJ)));
}

DEFINE_PROC(L"datum->syntax")
{
    RETURN(make_syntax(pair_cadr(F_SUBJ), syntax_wrap(pair_car(F_SUBJ))));
}

/*
 * r6rs-lib 12.7.  Generating lists of temporaries
 *
 * (generate-temporaries l)			# procedure
 */ 

/*
 * r6rs-lib 12.8.  Derived forms and procedures
 *
 * (with-syntax ((<pattern> <expression>) ...) <body>)
 *						# syntax
 *
 * (quasisyntax <template>)			# syntax
 * unsyntax					# auxiliary syntax
 * unsyntax-splicing				# auxiliary syntax
 */ 

/*
 * r6rs-lib 12.9.  Syntax violations
 *
 * (syntax-violation who message form)		# procedure
 * (syntax-violation who message form subform)	# procedure
 */
