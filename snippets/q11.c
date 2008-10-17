/* can you dereference an odd pointer? */

#include <stdio.h>

char a[] = "abcdefgh";

int main()
{
    int *p = (int *) (a + 3);
    printf("p = %p\n", p);
    *p = 42;
    printf("*p = %d\n", *p);
    printf("a = %s\n", a);
    return 0;
}
