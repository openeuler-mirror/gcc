/* { dg-do compile } */
/* { dg-options "-march=rv32gcv_zvl128b -mabi=ilp32d -mrvv-vector-bits=zvl" { target { rv32 } } } */
/* { dg-options "-march=rv64gcv_zvl128b -mabi=lp64d  -mrvv-vector-bits=zvl" { target { rv64 } } } */

#include <riscv_vector.h>

/* Sizeless objects with global scope.  */

vint8m1_t global_rvv_sc;
static vint8m1_t local_rvv_sc;
extern vint8m1_t extern_rvv_sc;
__thread vint8m1_t tls_rvv_sc;
_Atomic vint8m1_t atomic_rvv_sc;

struct rvv_fixed
{
  vfloat32m1_t a;
  vfloat32m1_t b;
}x;
