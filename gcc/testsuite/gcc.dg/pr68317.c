/* { dg-do compile } */
/* { dg-options "-O2 -fdisable-tree-ethread -fchrec-mul-fold-strict-overflow" } */

/* Note: Threader will collapse loop.  */

typedef int int32_t __attribute__((mode (__SI__)));

void bar (int32_t);

void
foo ()
{
 int32_t index = 0;

 for (index; index <= 10; index--) /* { dg-warning "iteration \[0-9\]+ invokes undefined behavior" } */
   /* Result of the following multiply will overflow
      when converted to signed int32_t.  */
   bar ((0xcafe + index) * 0xdead);
}
