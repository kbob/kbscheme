#include <stdlib.h>

/* Can a macro's arg list span lines? */

#define MACRO(a,\
              b) \
              a ## b

int main()
{
    MACRO(ex, it)(0);
}
