/* { dg-do compile { target { aarch64*-*-linux* } } } */
/* { dg-options "-O -fdump-rtl-expand-details -fccmp2" } */

int func (int a, int b, int c)
{
  while(1)
    {
      if(a-- == 0 || b >= c)
	{
	  return 1;
	}
    }
}

/* { dg-final { scan-assembler-times "\tccmp\t" 1} } */
