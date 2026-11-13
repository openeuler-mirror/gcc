/* { dg-do compile } */
/* { dg-options "-Og -g -fno-ipa-value-specialization -fno-ipa-value-specialization-cold" } */

extern void first_path (void);
extern void second_path (void);
extern void observe_value (int);
volatile int merge_path_sink;

/* Keep an unrelated integer predicate live so a failed reconstruction
   proof also visits the floating conditional edges.  Floating unordered
   comparisons must never be inverted with reverse_condition.  */
#define CHECK(NAME, COND)                                        \
void NAME (double x, double y, int n, int path)                   \
{                                                               \
  if (path) first_path (); else second_path ();                  \
  if (COND) first_path (); else second_path ();                  \
  int b = -3;                                                   \
  observe_value (b);                                             \
  if (n > 0) b = 5;                                             \
  observe_value (b);                                             \
  merge_path_sink = path;                                        \
  __asm__ volatile ("" ::: "memory");                             \
}

CHECK (ordered_less, __builtin_expect (x < y, 1))
CHECK (ordered_greater_equal, __builtin_expect (x >= y, 1))
CHECK (unordered_less, !(x >= y))
CHECK (unordered_greater, !(x <= y))
CHECK (unordered_equal, !__builtin_islessgreater (x, y))
CHECK (ordered_not_equal, __builtin_islessgreater (x, y))
