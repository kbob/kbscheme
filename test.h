#ifndef TEST_INCLUDED
#define TEST_INCLUDED

#include <stddef.h>			/* for wchar_t */

#include "concat.h"

#define TEST_READ(input, expected) DEFINE_TEST_CASE_(TP_READ, input, expected)
#define TEST_EVAL(input, expected) DEFINE_TEST_CASE_(TP_EVAL, input, expected)

#define DEFINE_TEST_CASE_(phase, input, expected)			\
__attribute__((constructor))						\
static void CAT_(register_##phase##_test_, __LINE__)(void)		\
{									\
    static test_case_t tc = {						\
	phase,								\
	input,								\
	expected,							\
	__FILE__,							\
	__LINE__,							\
	NULL								\
    };									\
    register_test(&tc);							\
}

typedef enum test_phase {
    TP_READ,
    TP_EVAL,
} test_phase_t;

typedef struct test_case test_case_t;
struct test_case {
    test_phase_t   tc_phase;
    const wchar_t *tc_input;
    const wchar_t *tc_expected;
    const char    *tc_file;
    int            tc_lineno;
    test_case_t   *tc_next;
};

extern void register_test(test_case_t *);
extern void self_test();

#endif /* !TEST_INCLUDED */
