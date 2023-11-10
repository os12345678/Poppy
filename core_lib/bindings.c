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

/**
 * The function "print" takes a format string and variable arguments, and prints
 * the formatted output to the console.
 * 
 * @param format The format parameter is a string that specifies the format of the
 * output. It can contain format specifiers that are replaced by the values
 * specified in the subsequent arguments.
 * 
 * @return the value of `retval`, which is the return value of the `vprintf`
 * function.
 */
int print(const char *format, ...) {
    va_list args;
    va_start(args, format);
    int retval = vprintf(format, args);
    va_end(args);
    return retval;
}

/**
 * The function GC_malloc is a wrapper for the malloc function in C.
 * 
 * @param size The size parameter is the number of bytes to allocate for the memory
 * block.
 * 
 * @return a pointer to the memory block that has been allocated using the malloc
 * function.
 */
void* GC_malloc(size_t size) {
    return malloc(size);
}

/**
 * The function "thread_create" creates a new thread using the pthread library in
 * C.
 * 
 * @param thread A pointer to a pthread_t variable where the thread ID will be
 * stored after the thread is created.
 * @param start_routine The start_routine is a pointer to the function that will be
 * executed by the new thread. It takes a single void pointer argument and returns
 * a void pointer. This function will be called when the new thread is created.
 * @param arg The "arg" parameter is a void pointer that can be used to pass any
 * additional arguments to the thread's start routine. The start routine is the
 * function that will be executed by the thread when it starts.
 * 
 * @return the result of the `pthread_create` function.
 */
int thread_create(pthread_t * thread, void * (*start_routine)(void *), void * arg) {
    return pthread_create(thread, NULL, start_routine, arg);
}

/**
 * The function `thread_join` is a wrapper for the `pthread_join` function in C.
 * 
 * @param thread The "thread" parameter is of type pthread_t and represents the
 * thread to be joined. It is the identifier of the thread that you want to wait
 * for.
 * @param value_ptr The value_ptr parameter is a pointer to a location where the
 * exit status of the joined thread will be stored.
 * 
 * @return the result of the pthread_join() function.
 */
int thread_join(pthread_t thread, void **value_ptr) {
    return pthread_join(thread, value_ptr);
}

/**
 * The function `pthread_equal_wrapper` is a wrapper function that calls the
 * `pthread_equal` function and returns its result.
 * 
 * @param t1 The first parameter, t1, is of type pthread_t. It represents the first
 * thread identifier that we want to compare.
 * @param t2 The parameter "t2" is of type "pthread_t", which is a data type used
 * to represent a thread identifier in the POSIX threads library.
 * 
 * @return the result of the pthread_equal function, which is an integer value
 * indicating whether the two pthread_t variables t1 and t2 are equal.
 */
int pthread_equal_wrapper(pthread_t t1, pthread_t t2) {
    return pthread_equal(t1, t2);
}

/**
 * The function `pthread_self_wrapper` returns the thread ID of the calling thread.
 * 
 * @return The function `pthread_self_wrapper` returns the thread ID of the calling
 * thread.
 */
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
