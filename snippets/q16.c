/* Are constructors and destructors called in any special order? */

#include <stdio.h>

__attribute__((constructor)) static void cons_one(void)
{
    printf("construct one\n");
}

__attribute__((destructor)) static void del_one(void)
{
    printf("destruct one\n");
}

__attribute__((constructor)) static void cons_two(void)
{
    printf("construct two\n");
}

__attribute__((destructor)) static void del_two(void)
{
    printf("destruct two\n");
}

int main()
{
    printf("main\n");
    return 0;
}

__attribute__((constructor)) static void cons_three(void)
{
    printf("construct three\n");
}

__attribute__((destructor)) static void del_three(void)
{
    printf("destruct three\n");
}
