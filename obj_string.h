#ifndef OBJ_STRING_INCLUDED
#define OBJ_STRING_INCLUDED

#include "obj.h"

extern obj_t         *make_string           (size_t len, wchar_t wc);
extern obj_t         *make_string_from_chars(const wchar_t *value, size_t len);
extern bool           is_string             (obj_t *);
extern size_t         string_len            (obj_t *);
extern const wchar_t *string_value          (obj_t *);
extern void           string_set_char       (obj_t *,
					     size_t  index,
					     wchar_t wc);
extern void           string_set_substring  (obj_t         *string,
					     size_t         pos,
					     size_t         len,
					     const wchar_t *substring);
extern int            strings_cmp           (obj_t *str1, obj_t *str2);

static inline bool    strings_are_equal     (obj_t *str1, obj_t *str2)
{
    return strings_cmp(str1, str2) == 0;
}

#endif /* !OBJ_STRING_INCLUDED */
