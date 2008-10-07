#include <stdio.h>

static int x;

int f()
{
    return x = 3, 4;
}

main()
{
    int y = f();
    printf("x = %d, y = %d\n", x, y);
}
