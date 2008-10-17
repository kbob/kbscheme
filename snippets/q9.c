/*
 * alloca with negative argument?
 */

#include <alloca.h>
#include <stdio.h>

void g(int n, int *p)
{
    int x;
    printf("alloca(%d): g at p - %d\n", n, (char *) p - (char *) &x);
}

void f(int n)
{
    void *p = alloca(n);
    g(n, &n);
    p = p;
}

int main()
{
    f(1);
    f(10);
    f(100);
    f(1000);
    f(-1);
    f(-10);
    f(-100);
    f(-1000);
    return 0;
}
