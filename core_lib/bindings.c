#include <stdio.h>
#include <stdarg.h>
#include <pthread.h>

// simple printf wrapper
void print(const char* format, long long *arg) {
  printf(format, *arg);
}

// create pthread
int create_thread(void* (*start_routine) (void *), void *arg) {
  pthread_t thread;
  return pthread_create(&thread, NULL, start_routine, arg);
}


// clang -S -emit-llvm core_lib/bindings.c -o core_lib/bindings.ll
