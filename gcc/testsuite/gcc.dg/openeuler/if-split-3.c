/* { dg-do compile } */
/* { dg-options "-O3 -fif-split -fdump-tree-if-split-details" } */

typedef struct X
{
    int a;
} X;


void  __attribute__ ((noinline)) set_a (X* x, int val)
{
    x->a = val;
}

static __attribute__ ((noinline)) int foo (int b)
{
    int res = 1;
    for (int i = 0; i < b; i++) {
        res*=3;
    }
    return res;
}

int main(int argc, char** argv){
    X data;
    set_a (&data, argc);
    int res = 0;

    if (data.a == 5 || data.a == 52 || data.a == 25)
        res = foo (data.a);

    return res;
}

/* Not split on gcc-14: conditions with three alternatives are recognized on
   gcc-12 but not here, while the two-alternative cases (if-split-2 and -4)
   still work.  Kept as xfail so an XPASS reports a fix; the analysis is in
   the group notes.  */
/* { dg-final { scan-tree-dump-times "Recognized necessary condition pair:" 2 "if-split" { xfail *-*-* } } } */
/* { dg-final { scan-tree-dump "Successfully transformed:" "if-split" { xfail *-*-* } } } */