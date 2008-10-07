#include <stdio.h>

int f(int a, int b, int c)
{
    a += 1;
    b += 2;
    c += 3;
    return a + b + c;
}

main()
{
    int c = f(10,20,30);
    printf("c = %d\n", c);
}
