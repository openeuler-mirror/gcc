/* { dg-do compile } */
/* { dg-options "-O3 -fif-split -fdump-tree-if-split-details" } */

typedef struct Y
{
    int b;
} Y;

typedef struct X
{
    Y y;
    int a;
} X;


void  __attribute__ ((noinline)) set_b (Y* y, int val)
{
    y->b = val;
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
    set_b (&data.y, argc);
    int res = 0;

    if (data.y.b == 5 || data.y.b == 52 || data.y.b == 25)
        res = foo (data.y.b);

    return res;
}

/* { dg-final { scan-tree-dump-times "Recognized necessary condition pair:" 2 "if-split" } } */
/* { dg-final { scan-tree-dump "Successfully transformed:" "if-split" } } */
