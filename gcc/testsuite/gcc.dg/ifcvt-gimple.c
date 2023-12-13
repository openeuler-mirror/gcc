/* { dg-do compile } */
/* { dg-options "-O2 -fif-conversion-gimple -fdump-tree-optimized -ftree-fold-phiopt" } */

int test_int (int optimizable_int) {
    if (optimizable_int > 5)
	++optimizable_int;
    return optimizable_int;
}

int test_int_pow2 (int optimizable_int_pow2) {
    if (optimizable_int_pow2 <= 4)
	optimizable_int_pow2 += 1024;
    return optimizable_int_pow2;
}

int test_int_non_pow2 (int not_optimizable_int_non_pow2) {
    if (not_optimizable_int_non_pow2 == 1)
	not_optimizable_int_non_pow2 += 513;
    return not_optimizable_int_non_pow2;
}

float test_float (float not_optimizable_float) {
    if (not_optimizable_float > 5)
	not_optimizable_float += 1;
    return not_optimizable_float;
}

/* Expecting if-else block in test_float and test_int_non_pow2 only. */
/* { dg-final { scan-tree-dump-not "if \\(optimizable" "optimized" } } */
/* { dg-final { scan-tree-dump "if \\(not_optimizable_int_non_pow2" "optimized" } } */
/* { dg-final { scan-tree-dump "if \\(not_optimizable_float" "optimized" } } */
/* { dg-final { scan-tree-dump-times "if " 2 "optimized" } } */
/* { dg-final { scan-tree-dump-times "else" 2 "optimized" } } */

/* Expecting shifted result only for optimizable_int_pow2. */
/* { dg-final { scan-tree-dump-times " << " 1 "optimized" } } */
/* { dg-final { scan-tree-dump " << 10;" "optimized" } } */
