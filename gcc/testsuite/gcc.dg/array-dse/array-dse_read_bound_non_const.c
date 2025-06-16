/* { dg-do compile } */

#include <stdio.h>
#include <stdlib.h>

void __attribute__((__noinline__)) test(int* a, unsigned long n) {
    for (int* p = a + (n - 1); p >= a; p--) {
        *p = p - a;
    }
}

int main() {
    int num[16];
    int n = 0;
    scanf("%d", &n);
    if (n)
        test(num, n);

    int n1;
    if (num[n1])
        abort();

    return 0;
}

/*--------------------------------------------------------------------------*/
/* { dg-final { scan-ipa-dump "Fail finding array dse candidate edges" "array-dse" } } */
