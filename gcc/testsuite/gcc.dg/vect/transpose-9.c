/* An unsuccessful temp-array insertion must release
   the loop vectorization info before later vectorizer passes run.  */
/* { dg-do compile { target aarch64*-*-linux* } } */
/* { dg-options "-O3 -ftree-slp-transpose-vectorize -fdump-tree-ldist-details" } */

typedef unsigned char u8;

struct result
{
  double values[7][7];
};

extern double input[6][6];

void
copy_rows (struct result *out)
{
  for (u8 row = 0; row < 6; ++row)
    for (u8 col = 0; col < 6; ++col)
      out->values[row][col] = input[row][col];
}

/* { dg-final { scan-tree-dump "failed to get iteration variable" "ldist" } } */
