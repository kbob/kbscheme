#include <stdlib.h>

typedef struct foo {
    void *f1;
    void *f2;
} foo_t;

void *f(foo_t **in)
{
    *in = (void *) 100;
    return (void *) 200;
}

foo_t g()
{
#if 0
    foo_t out = { (void *) 100, (void *) 200 };
    return out;
#else
    return (foo_t) { (void *) 100, (void *) 200 };
#endif
}

extern void x(void *, void *);

void callf()
{
    foo_t local;
    local.f1 = f((foo_t **) &local.f2);
    x(local.f1, local.f2);
}

void callg()
{
    extern void x(void *, void *);
    foo_t local = g();
    x(local.f1, local.f2);
}
