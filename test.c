#include "test.h"

#include <assert.h>
#include <stdlib.h>

#include "eval.h"
#include "io.h"
#include "lib.h"
#include "print.h"
#include "read.h"
#include "roots.h"

typedef int (*test_driver_t)(const test_case_t *);
static test_case_t *test_cases;

static char *phase_name(test_phase_t phase)
{
    switch (phase) {
    case TP_READ: return "read";
    case TP_EVAL: return "eval";
    default:      assert(0);
		  return NULL;
    }
}

static int read_driver(const test_case_t *tc)
{
    int err_count = 0;
    //printf("%s:%d read %ls\n", tc->tc_file, tc->tc_lineno, tc->tc_input);
    instream_t *in = make_string_instream(tc->tc_input, wcslen(tc->tc_input));
    obj_t *obj;
    bool ok = read_stream(in, &obj);
    assert(ok);
    const size_t out_size = 100;
    wchar_t actual[out_size + 1];
    outstream_t *out = make_string_outstream(actual, out_size);
    princ(obj, out);
    if (wcscmp(actual, tc->tc_expected)) {
	printf("%s:%d FAIL read test\n", tc->tc_file, tc->tc_lineno);
	printf("    input    = %ls\n", tc->tc_input);
        printf("    actual   = %ls\n", actual);
	printf("    expected = %ls\n", tc->tc_expected);
	printf("\n");
	err_count++;
    }
    return err_count;
}

static int eval_driver(const test_case_t *tc)
{
    /* XXX give each test a fresh environment. */

    int err_count = 0;
    //printf("%s:%d eval %ls\n", tc->tc_file, tc->tc_lineno, tc->tc_input);
    instream_t *in = make_string_instream(tc->tc_input, wcslen(tc->tc_input));
    AUTO_ROOT(expr, NIL);
    AUTO_ROOT(value, NIL);
    while (read_stream(in, &expr))
	value = eval(expr, library_env(r6rs_base_library()));
    /* Compare the value of the last expression. */
    const size_t out_size = 100;
    wchar_t actual[out_size + 1];
    outstream_t *out = make_string_outstream(actual, out_size);
    princ(value, out);
    if (wcscmp(actual, tc->tc_expected)) {
	printf("%s:%d FAIL eval test\n", tc->tc_file, tc->tc_lineno);
	printf("    input    = %ls\n", tc->tc_input);
        printf("    actual   = %ls\n", actual);
	printf("    expected = %ls\n", tc->tc_expected);
	printf("\n");
	err_count++;
    }
    POP_FUNCTION_ROOTS();
    return err_count;
}

static void test_all(test_phase_t phase, test_driver_t driver)
{
    int err_count = 0;
    int test_count = 0;
    test_case_t *tc;
    for (tc = test_cases; tc; tc = tc->tc_next)
	if (tc->tc_phase == phase) {
	    test_count++;
	    err_count += (*driver)(tc);
	}
    if (err_count) {
	fprintf(stderr, "%d error%s in phase %s.  FAIL.\n",
		err_count, &"s"[err_count == 1], phase_name(phase));
	exit(1);
    } else
	printf("  %3d %s tests passed.\n", test_count, phase_name(phase));
}

void register_test(test_case_t *tc)
{
    tc->tc_next = test_cases;
    test_cases = tc;
}

void self_test()
{
    test_all(TP_READ, read_driver);
    test_all(TP_EVAL, eval_driver);
}
