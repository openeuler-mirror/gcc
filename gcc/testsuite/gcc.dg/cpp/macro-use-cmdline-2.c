/*
   { dg-options "-fmacro-use-commandline -DTEST_MACRO=1" }
   { dg-do compile }
   { dg-do run }
*/

#define TEST_MACRO 300
#define TEST_MACRO_1 400
/*
   { dg-warning "-:redefined" "redef TEST_MACRO"      { target *-*-* } 7  }
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
  if (macroValue != 1)
    err("macroValue");

  int macroValue1 = TEST_MACRO_1;
  if (macroValue1 != 400)
    err("macroValue1");
  return 0;
}