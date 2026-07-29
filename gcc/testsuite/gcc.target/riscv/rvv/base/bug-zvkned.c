/* { dg-do assemble { target { rv64 } } } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-O3" "-Og" "-Os" "-Oz" } } */
/* { dg-options "-march=rv64gcv_zvkned -mabi=lp64d -O2 -save-temps" } */

#include <riscv_vector.h>

vuint32m8_t foo (vuint32m8_t vd, vuint32m1_t vs2, size_t vl)
{
  return __riscv_vaesef_vs_u32m1_u32m8 (vd, vs2, vl);
}

/* { dg-final { scan-assembler-times "vsetvli.*e32,m8" 1 } } */
