/* { dg-do run { target aarch64*-*-* } } */
/* { dg-options "-O2 --param max-rtl-if-conversion-unpredictable-cost=100 --param max-rtl-if-conversion-predictable-cost=100 --param=ifcvt-allow-register-renaming=2 -fifcvt-allow-complicated-cmps" } */

/* Correctness check for the register renaming: without the options below the
   renaming never runs, so the test as shipped exercised nothing specific to
   this feature.  The options are the same set the companion dump test uses.  */


extern void abort(void);

__attribute__ ((noinline))
int foo (int x, int y, int z, int a, int b)
{
  if (a < 2) {
      if (a == 0) {
	  if (x - y < 0)
	    x = x - y + z;
	  else
	    x = x - y;
	}
      else {
	  if (x + y >= z)
	    x = x + y - z;
	  else
	    x = x + y;
	}
    }
  return x;
}

int main(void) {
  if (foo (5,10,7,0,1) != 2) // x - y + z = -5 + 7 = 2
    abort ();
  if (foo (50,10,7,0,1) != 40) // x - y = 40
    abort ();
  if (foo (5,10,7,1,1) != 8) // x + y - z = 5 + 10 - 7 = 8
    abort ();
  if (foo (5,10,70,1,1) != 15) // x + y = 15
    abort ();
  return 0;
}

