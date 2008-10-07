#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>

__thread int x = 3;

void *target(void *unused)
{
    printf("x = %d; &x = %p\n", x, &x);
    return 0;
}

main()
{
    pthread_t thread;
    void *ret;
    int i;

    for (i = 0; i < 3; i++)
	if (pthread_create(&thread, NULL, target, NULL))
	    perror("pthread_create"), exit(1);
    for (i = 0; i < 3; i++)
	if (pthread_join(thread, &ret))
	    perror("pthread_join"), exit(1);
    return 0;
}


