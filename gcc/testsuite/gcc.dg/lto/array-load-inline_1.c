unsigned tbl[16];
void __attribute__((noinline))
init (void)
{
  for (unsigned i = 0; i < 16; ++i)
    tbl[i] = i * 11 + 7;
}
