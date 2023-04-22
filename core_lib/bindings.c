#include <stdio.h>
#include <stdarg.h>

// simple printf wrapper
void print(const char* format, long long arg) {
  printf(format, arg);
}

// clang -S -emit-llvm core_lib/bindings.c -o core_lib/bindings.ll
