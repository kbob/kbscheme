#include "charbuf.h"

#include <wchar.h>

#include "obj_string.h"
#include "roots.h"

void init_charbuf(charbuf_t *cbp, const wchar_t *prefix)
{
    size_t pos = wcslen(prefix);
    size_t len = 16;
    while (len < pos)
	len *= 2;
    cbp->cb_buf = make_string(len, L'\0');
    string_set_substring(cbp->cb_buf, 0, pos, prefix);
    cbp->cb_pos = pos;
    cbp->cb_len = len;
}

void charbuf_append_char(charbuf_t *cbp, wchar_t wc)
{
    if (cbp->cb_len == cbp->cb_pos) {
	cbp->cb_len *= 2;
	AUTO_ROOT(buf, cbp->cb_buf);
	obj_t *tmp = make_string(cbp->cb_len, L'\0');
	size_t i, pos = cbp->cb_pos;
	for (i = 0; i < pos; i++)
	    string_set_char(tmp, i, string_value(buf)[i]);
	POP_ROOT(buf);
	cbp->cb_buf = tmp;
    }
    string_set_char(cbp->cb_buf, cbp->cb_pos++, wc);
}

size_t charbuf_length(const charbuf_t *cbp)
{
    return cbp->cb_pos;
}

const wchar_t *charbuf_C_str(const charbuf_t *cbp)
{
    return string_value(cbp->cb_buf);
}

obj_t *charbuf_make_string(charbuf_t *cbp)
{
    PUSH_ROOT(cbp->cb_buf);
    const wchar_t *p = string_value(cbp->cb_buf);
    obj_t *string = make_string_from_chars(p, cbp->cb_pos);
    POP_ROOT(cbp->cb_buf);
    return string;
}
