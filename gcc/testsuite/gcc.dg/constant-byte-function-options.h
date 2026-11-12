static const char option_text[] = "#aa #bb #cc";
#ifndef PROBE_ATTRIBUTES
#define PROBE_ATTRIBUTES noipa
#endif
#ifdef PRAGMA_DISABLE
#pragma GCC push_options
#pragma GCC optimize ("no-tree-constant-byte-range", "no-tree-constant-byte-range-cursor")
#endif
__attribute__((PROBE_ATTRIBUTES))
unsigned byte_range_option_probe (unsigned start)
{
  const char *p = __builtin_strchr (option_text + start, ' ');
  return p ? (unsigned char) p[1] : 0;
}
#ifdef PRAGMA_DISABLE
#pragma GCC pop_options
#endif
/* An enabled control keeps both dump files available when the probe is off.  */
__attribute__((noipa)) unsigned byte_range_option_control (unsigned start)
{
  return start + 1;
}
#ifdef INCLUDE_MAIN
int main (void)
{
  for (unsigned i = 0; i != sizeof (option_text); ++i)
    {
      const char *p = __builtin_strchr (option_text + i, ' ');
      unsigned expect = p ? (unsigned char) p[1] : 0;
      if (byte_range_option_probe (i) != expect
          || byte_range_option_control (i) != i + 1)
        __builtin_abort ();
    }
  return 0;
}
#endif
