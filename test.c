#include "test.h"

#include <stdlib.h>

#include "eval.h"
#include "io.h"
#include "lib.h"
#include "print.h"
#include "read.h"

typedef struct eval_test_case {
    const wchar_t *etc_input;
    const wchar_t *etc_expected;
} eval_test_case_t;

eval_test_case_t eval_cases[] = {
    { L"()", L"()" },
    { L"123", L"123" },
    { L"+", L"#<proc-C>" },
    { L"(+)", L"0" },
    { L"(+ 3)", L"3" },
    { L"(+ 3 4)", L"7" },
    { L"(+ (+ 1 2) (+ 3 4))", L"10" },
    { L"((lambda (x) (+ x 3)) 4)", L"7" },
    { L"((lambda (x) (+ x x)) 4)", L"8" },
    { L"((lambda (x) (+) (+ x 3)) 4)", L"7" },
    { L"((lambda (x) (+ x 3) (+)) 4)", L"0" },

    { L"(if (= 0 0) 1 2)", L"1" },
    { L"(if (= 0 1) 1 2)", L"2" },
    { L"(if (= 0 0) 1)", L"1" },
    { L"(if (= 0 1) 1)", L"()" },

    { L"(define v0 3)", L"()" },
    { L"v0", L"3" },
    { L"(define v1)", L"()" },
    { L"v1", L"()" },
    { L"(define (v2 x) (- x))", L"()" },
    { L"v2", L"(lambda (x) (- x))" },
    { L"(v2 3)", L"-3" },
 /* { L"(define (v4 . x) x)", L"()" }, */
 /* { L"(v4)", L"()" }, */
 /* { L"(v4 3)", L"(3)" }, */
 /* Test that var can't be redefined. (need exceptions first.) */
    
    { L"(define-syntax qot (lambda (x) x))", L"()" },
    { L"(qot (1 2))", L"(1 2)" },

    { L"(quote ())", L"()" },
    { L"(quote (a b c))", L"(a b c)" },

    { L"(set! v1 4)", L"()" },
    { L"v1", L"4" },

    { L"(not (= 0 0))", L"#f" },
    { L"(not (= 1 0))", L"#t" },
    { L"(not +)", L"#f" },
    { L"(pair? (quote (1 2)))", L"#t" },
    { L"(pair? 12)", L"#f" },
    { L"(pair? ())", L"#f" },
    { L"(null? ())", L"#t" },
    { L"(null? +)", L"#f" },
    { L"(null? (quote (1 2)))", L"#f" },
    { L"(cons 1 2)", L"(1 . 2)" },
    { L"(cons 1 ())", L"(1)" },
    { L"(cons (quote (1 2)) (quote (3 4)))", L"((1 2) 3 4)" },
    { L"(car (quote (1 2)))", L"1" },
    { L"(cdr (quote (1 2)))", L"(2)" },
};
const size_t eval_case_count = sizeof eval_cases / sizeof eval_cases[0];

static obj_t *read_string(const wchar_t *str)
{
    instream_t *ip = make_string_instream(str, wcslen(str));
    return yyread(ip);
}

static void test_eval()
{
    eval_test_case_t *tp;
    for (tp = eval_cases; tp < eval_cases + eval_case_count; tp++) {
	const size_t out_size = 100;
	obj_t *input = read_string(tp->etc_input);
	obj_t *value = eval(input, library_env(r6rs_base_library()));
	wchar_t actual[out_size];
	princ(value, make_string_outstream(actual, out_size));
	if (wcscmp(actual, tp->etc_expected)) {
	    printf("FAIL eval %ls => %ls, expected %ls\n",
		   tp->etc_input, actual, tp->etc_expected);
	    exit(1);
	} else {
	    printf("OK %ls => %ls\n", tp->etc_input, actual);
	}
    }
}

void self_test()
{
    test_eval();
}
