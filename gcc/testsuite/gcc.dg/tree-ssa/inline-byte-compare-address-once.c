/* { dg-do run } */
/* { dg-options "-O0 -finline-memcmp" } */

char left_data[64], right_data[64];
int left_calls, right_calls, length_calls;

__attribute__((noinline, noclone)) const char *
left_address (void)
{
  ++left_calls;
  return left_data + 4;
}

__attribute__((noinline, noclone)) const char *
right_address (void)
{
  ++right_calls;
  return right_data + 8;
}

__attribute__((noinline, noclone)) __SIZE_TYPE__
comparison_length (void)
{
  ++length_calls;
  return 9;
}

__attribute__((noinline, noclone)) int
compare_addresses (void)
{
  return __builtin_memcmp (left_address (), right_address (),
                           comparison_length ());
}

int
main (void)
{
  for (int i = 0; i < 9; ++i)
    left_data[i + 4] = right_data[i + 8] = (char) (i + 1);
  if (compare_addresses () != 0
      || left_calls != 1 || right_calls != 1 || length_calls != 1)
    __builtin_abort ();
  right_data[12] = 1;
  if (compare_addresses () <= 0
      || left_calls != 2 || right_calls != 2 || length_calls != 2)
    __builtin_abort ();
  return 0;
}
