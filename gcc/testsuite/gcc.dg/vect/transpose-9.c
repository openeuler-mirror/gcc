/* Exercise the failed iteration-variable lookup in temp-array insertion.
   This test does not reproduce the original SLP lifetime ICE: without
   the fix, the later vectorizer reanalyzes this loop and overwrites the
   stale loop->aux instead of deleting the old loop_vec_info.  The dump
   check below verifies only that the early return is reached.  */
/* { dg-do compile { target aarch64*-*-linux* } } */
/* { dg-options "-O3 -ftree-slp-transpose-vectorize -fdump-tree-ldist-details" } */

/* The byte-sized row IV is cast to int before it is used as an array
   index.  The index checked by get_iv_from_seed is therefore not the
   row PHI result.  The differing row strides are needed in this test:
   equal strides prevent loop_vinfo creation before that check is reached.  */
typedef unsigned char u8;

struct result
{
  double values[6][7];
};

extern double input[6][6];

void
copy_rows (struct result *out)
{
  for (u8 row = 0; row < 6; ++row)
    for (int col = 0; col < 6; ++col)
      out->values[row][col] = input[row][col];
}

/* { dg-final { scan-tree-dump "failed to get iteration variable" "ldist" } } */
