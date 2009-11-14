#include <setjmp.h>
#include <stdio.h>
#include <sys/time.h>

#define N 10000000

jmp_buf b;
sigjmp_buf s;

void jump_back()
{
    longjmp(b, 1);
}

void jump_about()
{
    int i;
    for (i = 0; i < N; i++)
	if (!setjmp(b))
	    jump_back();
}

void sig_jump_back()
{
    siglongjmp(s, 1);
}

void sig_jump_about()
{
    int i;
    for (i = 0; i < N; i++)
	if (!sigsetjmp(s, 1))
	    sig_jump_back();
}

double now()
{
    struct timeval tv;
    gettimeofday(&tv, NULL);
    return tv.tv_sec + tv.tv_usec / 1000000.0;
}

double profile(const char *label, void (*f)())
{
    double before, after, duration;
    before = now();
    f();
    after = now();
    duration = after - before;
    printf("%s: %d reps, %g seconds, %g usec/rep\n",
	   label, N, duration, duration / N * 1000000);
    return duration / N;
}

int main()
{
    double a = profile("setjmp", jump_about);
    double b = profile("sigsetjmp", sig_jump_about);
    printf("ratio %g\n", b / a);
    return 0;
}
