/* { dg-do compile } */
/* { dg-options "-O3 -fif-split -fdump-tree-if-split-details" } */

static __attribute__ ((noinline)) int foo (int b)
{
    int res = 1;
    for (int i = 0; i < b; i++) {
        res*=3;
    }
    return res;
}

int main(int argc, char** argv){
    int b = argc;
    int res = 0;

    if (b == 5 || b == 52)
        res = foo (b);

    return res;
}

/* { dg-final { scan-tree-dump-times "Recognized necessary condition pair:" 1 "if-split" } } */
/* { dg-final { scan-tree-dump "Successfully transformed:" "if-split" } } */