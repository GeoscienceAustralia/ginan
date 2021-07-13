/**
 * fthread.c
 *
 * pthread library interface to Fortran, for OS's supporting pthreads
 *
 * John Donovan, Geoscience Australia
 * 26 March 2020
 */

#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>

/* Defines for FORTRAN. NB the decorations are GFORTRAN compiler specific
 * Other compilers/platforms may need different ones and then will need to be some 
 * #ifdef <platform/compiler> #endif wrappers
 */
#define THREAD_CREATE thread_create_
#define THREAD_YIELD thread_yield_
#define THREAD_SELF thread_self_
#define THREAD_JOIN thread_join_
#define THREAD_EXIT thread_exit_
#define THREAD_LOCK thread_lock_
#define THREAD_UNLOCK thread_unlock_
#define THREAD_GET_MUTEX thread_get_mutex_
#define THREAD_RELEASE_MUTEX thread_release_mutex_

/*
 * THREAD_CREATE - create a new fortran thread 
 *
 * @param thread_func: function pointer: entry to the thread
 * @param thread_id: the thread identifier
 */
void THREAD_CREATE(void *(*thread_func)(void *), pthread_t *thread_id) {
	pthread_create(thread_id, NULL, thread_func, NULL);
}

#ifdef __USE_GNU
/*
 * THREAD_YIELD - yield control to other threads
 */
void THREAD_YIELD() {
	pthread_yield();
}
#endif

/*
 * THREAD_SELF - get my identifier
 */
pthread_t THREAD_SELF() {
	return pthread_self();
}

/* 
 * THREAD_JOIN - wait until the marked thread joins (ends)
 */
void THREAD_JOIN(pthread_t* theThread) {
	int value = 0;

	pthread_join(*theThread, (void **) &value);
}

/*
 * THREAD_EXIT - end the thread
 */
void THREAD_EXIT(void *status) {
	pthread_exit(status);
}

/*
 * THREAD_LOCK  - lock all threads on the mutex supplied
 */
void THREAD_LOCK(pthread_mutex_t **theMutex) {
	// printf("locking on mutex %x\n", *theMutex);
	pthread_mutex_lock(*theMutex);
}

/*
 * THREAD_UNLOCK - unlock all threads held on the mutex supplied
 */
void THREAD_UNLOCK(pthread_mutex_t **theMutex) {
	pthread_mutex_unlock(*theMutex);
}

/*
 * THREAD_GET_MUTEX - get a new mutex object
 */
int THREAD_GET_MUTEX(pthread_mutex_t **theMutex) {
	*theMutex = (pthread_mutex_t *) malloc(sizeof(pthread_mutex_t));
	if (*theMutex == NULL) {
		return 0;
	}
	pthread_mutex_init(*theMutex, NULL);
	return 1;
}

/* 
 * THREAD_RELEASE_MUTEX - release the mutex object
 */
void THREAD_RELEASE_MUTEX(pthread_mutex_t **theMutex) {
	pthread_mutex_destroy(*theMutex);
	free(*theMutex);
	*theMutex = NULL;
}
