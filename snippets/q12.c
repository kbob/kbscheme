#include <assert.h>

/* local variable with same name as arg? */

//#define A a

void f(int A)
{
    struct s {
	int f0;
    } a = { A };

    assert(a.f0 == 42);
}

int main()
{
    f(42);
    return 0;
}
