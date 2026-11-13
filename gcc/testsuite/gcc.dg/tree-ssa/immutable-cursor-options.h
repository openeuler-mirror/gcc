#include <string.h>

static inline __attribute__((always_inline)) unsigned
cursor_interpret (const unsigned char *data, const char *program)
{
  unsigned result = 0;
  while (*program)
    {
      if (*program == ':')
	{
	  ++program;
	  continue;
	}
      const char *next = strchr (program, ':');
      if (*program == '?')
	{
	  program += (*data & 1) ? 1 : 2;
	  if (*program == 'a')
	    result += data[1];
	  else if (*program == 'b')
	    result -= data[1];
	  else
	    result ^= data[1];
	}
      else if (*program == 'a')
	result += *data;
      else if (*program == 'b')
	result -= *data;
      else
	result ^= *data;
      ++data;
      if (!next)
	break;
      program = next + 1;
    }
  return result;
}

/* Keep a reachable positive in the same dump even when the command-line
   cursor or threading option is disabled.  */
__attribute__((noipa, optimize("tree-immutable-loop-specialization-cursor",
			     "thread-jumps")))
unsigned
cursor_enabled (const unsigned char *data)
{
  return cursor_interpret (data, "?ab:a:b:?ba");
}

__attribute__((noipa, optimize("no-tree-immutable-loop-specialization-cursor")))
unsigned
cursor_disabled (const unsigned char *data)
{
  return cursor_interpret (data, "?ab:a:b:?ba");
}

__attribute__((noipa)) unsigned
cursor_plain (const unsigned char *data)
{
  return cursor_interpret (data, "?ab:a:b:?ba");
}

int
main (void)
{
  unsigned char data[8];
  char dynamic_program[32];
  strcpy (dynamic_program, "?ab:a:b:?ba");
  __asm__ volatile ("" : "+m" (dynamic_program));
  for (unsigned i = 0; i < 256; ++i)
    {
      for (unsigned j = 0; j < 8; ++j)
	data[j] = (i + j * 13) & 255;
      unsigned expected = cursor_interpret (data, dynamic_program);
      if (cursor_enabled (data) != expected
	  || cursor_disabled (data) != expected
	  || cursor_plain (data) != expected)
	__builtin_abort ();
    }
  return 0;
}
