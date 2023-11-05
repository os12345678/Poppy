#include <stdarg.h>
#include <stdio.h>
#include <pthread.h>
#include <stdlib.h>

void* GC_malloc(size_t size);
int thread_create(pthread_t * thread, void * (*start_routine)(void *), void * arg);
int thread_join(pthread_t thread, void **value_ptr);
int pthread_equal_wrapper(pthread_t t1, pthread_t t2);
pthread_t pthread_self_wrapper(void);
int print(const char *format, ...);

int print(const char *format, ...) {
    va_list args;
    va_start(args, format);
    int retval = vprintf(format, args);
    va_end(args);
    return retval;
}

void* GC_malloc(size_t size) {
    return malloc(size);
}

int thread_create(pthread_t * thread, void * (*start_routine)(void *), void * arg) {
    return pthread_create(thread, NULL, start_routine, arg);
}

int thread_join(pthread_t thread, void **value_ptr) {
    return pthread_join(thread, value_ptr);
}

int pthread_equal_wrapper(pthread_t t1, pthread_t t2) {
    return pthread_equal(t1, t2);
}

pthread_t pthread_self_wrapper(void) {
    return pthread_self();
}


// // create pthread and return thread identifier
// pthread_t create_thread(void* (*start_routine) (void *), void *arg) {
//   pthread_t thread;
//   pthread_create(&thread, NULL, start_routine, arg);
//   return thread;
// }

// // join a thread with the given identifier
// int join_thread(pthread_t thread) {
//   return pthread_join(thread, NULL);
// }

// clang -S -emit-llvm core_lib/bindings.c -o core_lib/bindings.ll
