/* { dg-do compile } */
/* { dg-options "-O2 -march=armv9-a+sme -mtrack-speculation" } */

/* aarch64_restore_za expanded a CBNZ by name, and that pattern is disabled
   by -mtrack-speculation.  Restoring ZA after a call that committed the
   lazy save is the path that reaches it.  */

void private_za (void);
void out_za (void) __arm_out ("za");
void in_za (void) __arm_in ("za");

__arm_new ("za") void
caller (void)
{
  out_za ();
  private_za ();
  in_za ();
}

/* { dg-final { scan-assembler {\tbl\t__arm_tpidr2_restore} } } */
/* { dg-final { scan-assembler-not "\tcbz\t" } } */
/* { dg-final { scan-assembler-not "\tcbnz\t" } } */
