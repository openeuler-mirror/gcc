/* { dg-do compile { target { aarch64*-*-linux* } } } */
/* { dg-additional-options "-mtune=tsv110 --param=tree-forwprop-perm=1 -fdump-tree-forwprop-details" } */
/* { dg-require-effective-target vect_int } */
#include <stdio.h>
#include <stdlib.h>
#include "tree-vect.h"

typedef unsigned short int sum_t;
typedef unsigned int sum2_t;
typedef long int intptr_t;
typedef unsigned char data;
#define BITS_PER_SUM (8 * sizeof(sum_t))

static sum2_t bar(sum2_t a )
{
    sum2_t s = ((a>>(BITS_PER_SUM-1))&(((sum2_t)1<<BITS_PER_SUM)+1))*((sum_t)-1);
    return (a+s)^s;
}

int foo(data *pix1, intptr_t i_pix1, data *pix2, intptr_t i_pix2 )
{
    sum2_t tmp[4][4];
    sum2_t a0, a1, a2, a3;
    sum2_t sum = 0;
    for( int i = 0; i < 4; i++, pix1 += i_pix1, pix2 += i_pix2 )
    {
        a0 = (pix1[0] - pix2[0]) + ((sum2_t)(pix1[4] - pix2[4]) << BITS_PER_SUM);
        a1 = (pix1[1] - pix2[1]) + ((sum2_t)(pix1[5] - pix2[5]) << BITS_PER_SUM);
        a2 = (pix1[2] - pix2[2]) + ((sum2_t)(pix1[6] - pix2[6]) << BITS_PER_SUM);
        a3 = (pix1[3] - pix2[3]) + ((sum2_t)(pix1[7] - pix2[7]) << BITS_PER_SUM);
        sum2_t t0 = a0 + a1;
        sum2_t t1 = a0 - a1;
        sum2_t t2 = a2 + a3;
        sum2_t t3 = a2 - a3;
        tmp[i][0] = t0 + t2;
        tmp[i][2] = t0 - t2;
        tmp[i][1] = t1 + t3;
        tmp[i][3] = t1 - t3;
    }
    for( int i = 0; i < 4; i++ )
    {
        sum2_t t0 = tmp[0][i] + tmp[1][i];
        sum2_t t1 = tmp[0][i] - tmp[1][i];
        sum2_t t2 = tmp[2][i] + tmp[3][i];
        sum2_t t3 = tmp[2][i] - tmp[3][i];
        a0 = t0 + t2;
        a2 = t0 - t2;
        a1 = t1 + t3;
        a3 = t1 - t3;
        sum += bar(a0) + bar(a1) + bar(a2) + bar(a3);
    }
    return (((sum_t)sum) + (sum>>BITS_PER_SUM)) >> 1;
}
/* { dg-final { scan-tree-dump "Initial permutations were reduced:" "forwprop4" } } */
/* { dg-final { scan-tree-dump "Permutations were moved through binary operations:" "forwprop4" } } */

