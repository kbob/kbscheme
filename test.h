#ifndef TEST_INCLUDED
#define TEST_INCLUDED

#include <stddef.h>			/* for wchar_t */

#include "uniq.h"

#define TEST_READ(input, expected) DEFINE_TEST_CASE_(TP_READ, input, expected)
#define TEST_EVAL(input, expected) DEFINE_TEST_CASE_(TP_EVAL, input, expected)

#define DEFINE_TEST_CASE_(phase, input, expected)			\
__attribute__((constructor))						\
static void UNIQ_IDENT(register_##phase##_test_)(void)			\
{									\
    static test_case_descriptor_t tc = {				\
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

typedef struct test_case_descriptor test_case_descriptor_t;
struct test_case_descriptor {
    test_phase_t            tcd_phase;
    const wchar_t          *tcd_input;
    const wchar_t          *tcd_expected;
    const char             *tcd_file;
    int                     tcd_lineno;
    test_case_descriptor_t *tcd_next;
};

extern void register_test(test_case_descriptor_t *);
extern void self_test();

#endif /* !TEST_INCLUDED */
