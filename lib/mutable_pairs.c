#include "proc.h"
#include "test.h"

LIBRARY(L"(rnrs mutable-pairs (6))")

/*
 * 17. Mutables pairs
 *
 * (set-car! pair obj)			# procedure
 * (set-cdr! pair obj)			# procedure
 */

DEFINE_PROC(L"set-car!")
{
    obj_t *pair = pair_car(F_SUBJ);
    obj_t *obj = pair_cadr(F_SUBJ);
    pair_set_car(pair, obj);
    RETURN(UNSPECIFIED);
}

DEFINE_PROC(L"set-cdr!")
{
    obj_t *pair = pair_car(F_SUBJ);
    obj_t *obj = pair_cadr(F_SUBJ);
    pair_set_cdr(pair, obj);
    RETURN(UNSPECIFIED);
}

TEST_EVAL(L"(define a \'(a . b))"
	  L"(set-car! a \'c)"
	  L"a",				L"(c . b)");

TEST_EVAL(L"(define a \'(a . b))"
	  L"(set-cdr! a \'c)"
	  L"a",				L"(a . c)");

/* from r6rs */
TEST_EVAL(L"(define (f) (list 'not-a-constant-list))\n"
	  L"(set-car! (f) 3)\n",	UNSPECIFIED_REPR);
//TEST_EVAL(L"(define (g) '(constant-list))\n"
//	  L"(set-car! (g) 3)\n"
//	  L"(g)",			L"&assertion");
//TEST_EVAL(L"(let ((x (list 'a 'b 'c 'a))\n"
//	  L"      (y (list 'a 'b 'c 'a b 'c 'a)))\n"
//	  L"  (set-cdr! (list-tail x 2) x)\n"
//	  L"  (set-cdr! (list-tail y 5) y)\n"
//	  L"  (list\n"
//	  L"    (equal? x x)\n"
//	  L"    (equal? x y)\n"
//	  L"    (equal? (list x y 'a) list y x 'b))))",
//					L"(#t #t #f)");
