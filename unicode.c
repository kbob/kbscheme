#include "unicode.h"

#include <stdio.h>

#include "bool.h"

typedef struct unicode_data {
#if 0
    enum unicode_general_category_t  ud_general_category:5;
    bool                             ud_alphabetic:1;
    bool                             ud_is_numeric:1;
    bool                             ud_is_whitespace:1;
    bool                             ud_is_upper_case:1;
    bool                             ud_is_lower_case:1;
    bool                             ud_is_title_case:1;
    wchar_t                          ud_simple_upcase;
    wchar_t                          ud_simple_downcase;
    wchar_t                          ud_simple_titlecase;
    wchar_t                          ud_simple_foldcase;
    wchar_t                         *ud_full_upcase;
    wchar_t                         *ud_full_downcase;
    wchar_t                         *ud_full_titlecase;
    wchar_t                         *ud_full_foldcase;
#else
    unsigned char ud_general_category;
#endif
} unicode_data_t;

#include "ucd_data.h"

static inline const unicode_data_t *get_data(wchar_t c)
{
    size_t pn = c / UCD_PAGE_SIZE;
    size_t pi = c % UCD_PAGE_SIZE;
    size_t i = ucd_page_map[pn] * UCD_PAGE_SIZE + pi;
    fflush(stdout);
    return &ucd_data[i];
}

unicode_general_category_t unicode_general_category(wchar_t c)
{
    if (c < 0 || c >= UCD_CODEPOINT_COUNT)
	return UGC_OTHER_NOT_ASSIGNED;
    return get_data(c)->ud_general_category;
}
