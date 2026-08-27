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

    if (data.a == 5 || data.a == 52)
        res = foo (data.a);

    return res;
}

/* { dg-final { scan-tree-dump-times "Recognized necessary condition pair:" 1 "if-split" } } */
/* { dg-final { scan-tree-dump "Successfully transformed:" "if-split" } } */
