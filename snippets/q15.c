#include <stdio.h>

int main()
{
    void *vp = 0;
    printf("sizeof (void) = %d\n", sizeof (void));
    printf("sizeof *(void *) = %d\n", sizeof *vp);
    printf("vp + 1 = %d\n", (int) (vp + 1));
    return 0;
}
