/* { dg-do run } */
/* { dg-options "-O3 -ftree-immutable-loop-specialization-cursor -fdump-tree-cursor-thread-details" } */
/* { dg-final { scan-tree-dump "Immutable cursor CFG changed: yes" "cursor-thread1" } } */
#include <stddef.h>
#include <string.h>

/* A small immutable command stream with data-dependent alternatives.  */
static inline __attribute__((always_inline))
int interpret(const unsigned char *data, const char *program)
{
  int result=0;
  while (*program) {
    if (*program == ':') {
      ++program;
      continue;
    }
    const char *next=strchr(program, ':');
    if (*program=='?') {
      if (*data & 1) ++program;
      else program+=2;
      if (*program=='a') result+=data[1];
      else if (*program=='b') result-=data[1];
      else result^=data[1];
    } else {
      if (*program=='a') result+=*data;
      else if (*program=='b') result-=*data;
      else result^=*data;
    }
    ++data;
    if (!next) break;
    program=next+1;
  }
  return result;
}

__attribute__((noinline)) int specialized(const unsigned char *p)
{
  return interpret(p,"?ab:a:b:?ba");
}

int main(void)
{
  unsigned char data[8];
  char dynamic_program[32];
  strcpy(dynamic_program,"?ab:a:b:?ba");
  asm volatile("" : "+m"(dynamic_program));
  for (int i=0;i<256;i++) {
    for (int j=0;j<8;j++) data[j]=(i+j*13)&255;
    if (specialized(data)!=interpret(data,dynamic_program)) __builtin_abort();
  }
  return 0;
}
