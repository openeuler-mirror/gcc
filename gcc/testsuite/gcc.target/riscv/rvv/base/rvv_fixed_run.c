/* { dg-do run { target { riscv_v } } }
/* { dg-options "-march=rv32gcv_zvl128b -mabi=ilp32d -mrvv-vector-bits=zvl" { target { rv32 } } } */
/* { dg-options "-march=rv64gcv_zvl128b -mabi=lp64d -mrvv-vector-bits=zvl" { target { rv64 } } } */

#include <stdio.h>
#include <riscv_vector.h>

int32_t a[16] = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16};

struct rvv_fixed
{
  size_t gvl;
  vint32m1_t va;
  vint32m1_t vb;
  vint32m1_t vc;
};

// Vector version.
void __attribute__((noinline)) add_vectorized(int32_t *c, int32_t *a, int32_t *b, int N) {
  struct rvv_fixed x;
  for (; N > 0; N -= x.gvl) {
    x.gvl = __riscv_vsetvl_e32m1(N);
     x.va = __riscv_vle32_v_i32m1(a, x.gvl);
    a += x.gvl;
     x.vb = __riscv_vle32_v_i32m1(b, x.gvl);
    b += x.gvl;
     x.vc = __riscv_vadd_vv_i32m1(x.va, x.vb, x.gvl);
     __riscv_vse32_v_i32m1(c, x.vc, x.gvl);
    c += x.gvl;
  }
}

int main() {
  int N = 16;
  int32_t vector[N];
  add_vectorized(vector, a, a, N);
  for (int i = 0; i < N; i++)
    printf("%d ", vector[i]);
  printf("\n");
  return 0;
}

/* { dg-output "2 4 6 8 10 12 14 16 18 20 22 24 26 28 30 32 \r\n" } */

