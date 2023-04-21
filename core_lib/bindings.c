#include <stdio.h>
#include <stdarg.h>

// simple printf wrapper
void print(const char* format, ...) {
  va_list args;
  va_start(args, format);
  vprintf(format, args);
  va_end(args);
}