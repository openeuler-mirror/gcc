
extern void abort(void);

__attribute__ ((noinline))
int foo (int x, int y, int z, int a, int b)
{
  if (a < 2)
    {
      if (a == 0)
	{
	  if (x - y < 0)
	    x = x - y + z;
	  else
	    x = x - y;
	}
      else
	{
	  if (x + y >= z)
	    x = x + y - z;
	  else
	    x = x + y;
	}
    }
  return x;
}

int main(void)
{
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
