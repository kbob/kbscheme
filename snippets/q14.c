#include <stdio.h>

void froogle(int n)
{
    printf("__func__ = %s\n", __func__);
    printf("__func__ = %p\n", __func__);
    printf("__func__ = %p\n", __func__);
    printf("__PRETTY_FUNCTION__ = %s\n", __PRETTY_FUNCTION__);
}

int main()
{
    froogle(3);
    return 0;
}
