#include <stdio.h>
#include <stdarg.h>
#include <pthread.h>

// printf 
void print(char format[static 1], ...) {
    va_list args;
    va_start(args, format);
    vprintf(format, args);
    va_end(args);
}

// create pthread and return thread identifier
pthread_t create_thread(void* (*start_routine) (void *), void *arg) {
  pthread_t thread;
  pthread_create(&thread, NULL, start_routine, arg);
  return thread;
}

// join a thread with the given identifier
int join_thread(pthread_t thread) {
  return pthread_join(thread, NULL);
}

// clang -S -emit-llvm core_lib/bindings.c -o core_lib/bindings.ll
