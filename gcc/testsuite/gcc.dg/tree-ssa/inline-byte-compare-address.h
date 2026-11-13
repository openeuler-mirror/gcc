/* Address forms accepted by the target can include symbolic constants.
   Optional memcmp fallbacks must stabilize them before rebuilding trees.  */

char address_global_a[64], address_global_b[64];
static char address_static[64];
extern char address_external[64];
struct address_record { int tag; char data[64]; } address_struct;

int
address_global_left (const void *p, __SIZE_TYPE__ n)
{
  return __builtin_memcmp (address_global_a + 4, p, n);
}

int
address_global_right (const void *p, __SIZE_TYPE__ n)
{
  return __builtin_memcmp (p, address_global_a + 4, n);
}

int
address_static_left (const void *p, __SIZE_TYPE__ n)
{
  return __builtin_memcmp (address_static + 4, p, n);
}

int
address_external_left (const void *p, __SIZE_TYPE__ n)
{
  return __builtin_memcmp (address_external + 4, p, n);
}

int
address_struct_member (const void *p, __SIZE_TYPE__ n)
{
  return __builtin_memcmp (address_struct.data, p, n);
}

int
address_both_offsets (__SIZE_TYPE__ n)
{
  return __builtin_memcmp (address_global_a + 4, address_global_b + 8, n);
}

int
address_equality (const void *p, __SIZE_TYPE__ n)
{
  return __builtin_memcmp (address_global_a + 4, p, n) == 0;
}

int
address_plain_control (const void *p, __SIZE_TYPE__ n)
{
  return __builtin_memcmp (address_global_a, p, n);
}

int
address_parameter_control (const char *p, const void *q, __SIZE_TYPE__ n)
{
  return __builtin_memcmp (p + 4, q, n);
}
