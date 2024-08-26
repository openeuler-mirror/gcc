/*
   { dg-options "-fmacro-use-commandline -DTEST_MACRO=1 -DTEST_MACRO=20" }
   { dg-do compile }
   { dg-do run }
*/

/* { dg-warning "-:redefined" "redef TEST_MACRO"      { target *-*-* } 0  }
   { dg-message "-:previous"  "prev def TEST_MACRO"   { target *-*-* } 0  }
*/

#if DEBUG
extern int puts (const char *);
#else
#define puts(X)
#endif
extern void abort (void);

#define err(str) do { puts(str); abort(); } while (0)

int main (int argc, char *argv[])
{
  int macroValue = TEST_MACRO;
  if (macroValue != 20)
    err("macroValue");
  return 0;
}