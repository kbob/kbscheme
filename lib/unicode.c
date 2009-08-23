#include <assert.h>

#include "proc.h"
#include "unicode.h"
#include "test.h"

LIBRARY(L"(rnrs unicode (6))");

/* 1.1 Characters
 *
 * (char-upcase char)			# procedure
 * (char-downcase char)			# procedure
 * (char-titlecase char)		# procedure
 * (char-foldcase char)			# procedure
 *
 * (char-ci=? char1 char2 char3 ...)	# procedure
 * (char-ci<? char1 char2 char3 ...)	# procedure
 * (char-ci>? char1 char2 char3 ...)	# procedure
 * (char-ci<=? char1 char2 char3 ...)	# procedure
 * (char-ci>=? char1 char2 char3 ...)	# procedure
 *
 * (char-alphabetic? char)		# procedure
 * (char-numeric? char)			# procedure
 * (char-whitespace? char)		# procedure
 * (char-upper-case? char)		# procedure
 * (char-lower-case? char)		# procedure
 * (char-title-case? char)		# procedure
 *
 * (char-general-category char)		# procedure
 */

/* from r6rs */
//TEST_EVAL(L"(char-upcase #\\i)",		L"#\\I");
//TEST_EVAL(L"(char-downcase #\\i)",		L"#\\i");
//TEST_EVAL(L"(char-titlecase #\\i)",		L"#\\I");
//TEST_EVAL(L"(char-foldcase #\\i)",		L"#\\i");

/* from r6rs */
//TEST_EVAL(L"(char-upcase #\\\xdf)",		L"#\\xdf");
//TEST_EVAL(L"(char-downcase #\\\xdf)",		L"#\\xdf");
//TEST_EVAL(L"(char-titlecase #\\\xdf)",	L"#\\xdf");
//TEST_EVAL(L"(char-foldcase #\\\xdf)",		L"#\\xdf");

/* from r6rs */
//TEST_EVAL(L"(char-upcase #\\\x03a3)",		L"#\\x0303");
//TEST_EVAL(L"(char-downcase #\\\x03a3)",	L"#\\x03c3");
//TEST_EVAL(L"(char-titlecase #\\\x3a3)",	L"#\\x0303");
//TEST_EVAL(L"(char-foldcase #\\\x03a3)",	L"#\\x03c3");

/* from r6rs */
//TEST_EVAL(L"(char-upcase #\\\x03c2)",		L"#\\x0303");
//TEST_EVAL(L"(char-downcase #\\\x03c2)",	L"#\\x03c2");
//TEST_EVAL(L"(char-titlecase #\\\x3c2)",	L"#\\x0303");
//TEST_EVAL(L"(char-foldcase #\\\x03c2)",	L"#\\x03c3");

/* from r6rs */
//TEST_EVAL(L"(char-ci<? #\\z #\\Z)",		L"#f");
//TEST_EVAL(L"(char-ci=? #\\z #\\Z)",		L"#t");
//TEST_EVAL(L"(char-ci=? #\\\x03c2 #\\\x03c3)", L"#t");

/* from r6rs */
//TEST_EVAL(L"(char-alphabetic? #\\a)",		L"#t");
//TEST_EVAL(L"(char-numeric? #\\1)",		L"#t");
//TEST_EVAL(L"(char-whitespace? #\\space)",	L"#t");
//TEST_EVAL(L"(char-whitespace? #\\x00A0)",	L"#t");
//TEST_EVAL(L"(char-upper-case? #\\\x0303)",	L"#t");
//TEST_EVAL(L"(char-lower-case? #\\\x03c3)",	L"#t");
//TEST_EVAL(L"(char-lower-case? #\\x00AA)",	L"#t");
//TEST_EVAL(L"(char-title-case? #\\I)",		L"#f");
//TEST_EVAL(L"(char-title-case? #\\x01C5)",	L"#t");

DEFINE_PROC(L"char-general-category")
{
    wchar_t c = character_value(pair_car(F_SUBJ));
    unicode_general_category_t cat = unicode_general_category(c);
    const wchar_t *short_name = unicode_gc_short_name(cat);
    RETURN(make_symbol(short_name));
}

/* from r6rs */
TEST_EVAL(L"(char-general-category #\\a)",       L"Ll");
TEST_EVAL(L"(char-general-category #\\space)",   L"Zs");
TEST_EVAL(L"(char-general-category #\\x10ffff)", L"Cn");
