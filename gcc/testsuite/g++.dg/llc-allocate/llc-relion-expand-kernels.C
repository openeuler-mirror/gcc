/* { dg-do compile { target { aarch64*-*-linux* } } } */
/* { dg-options "-O3 -march=armv8.2-a+sve -static -fllc-allocate -fdump-tree-llc_allocate-details-lineno --param branch-prob-threshold=50  --param filter-kernels=0 --param mem-access-num=2 --param issue-topn=1 --param force-issue=1" } */
#include "multidim_array.h"

class Input
{
  public:
    int metadata_offset = 13;
    int exp_nr_images = 1;
    MultidimArray<double> exp_Mweight;
    void convertAllSquaredDifferencesToWeights();
};

int main()
{
  clock_t start = clock();
  Input input;
  int testIter = 2;

  for (int i = 0; i < testIter; ++i)
    {
      input.convertAllSquaredDifferencesToWeights();
    }
  return 0;
}

void Input::convertAllSquaredDifferencesToWeights()
{
  for (int img_id = 0; img_id < exp_nr_images; img_id++)
  {
    int my_metadata_offset = metadata_offset + img_id;
    MultidimArray<double> sorted_weight;

    exp_Mweight.getRow(img_id, sorted_weight);
    long int np = 0;
    FOR_ALL_DIRECT_ELEMENTS_IN_MULTIDIMARRAY(sorted_weight)
    {
      if (DIRECT_MULTIDIM_ELEM(sorted_weight, n) > 0.)
        {
          DIRECT_MULTIDIM_ELEM(sorted_weight, np) = DIRECT_MULTIDIM_ELEM( \
            sorted_weight, n);
          np++;
        }
    }
  }
}



/* { dg-final { scan-tree-dump-times "dense memory access" 1 "llc_allocate" } } */
/* { dg-final { scan-tree-dump-times "__builtin_prefetch" 1 "llc_allocate" } } */
/* { dg-final { scan-tree-dump-times "static issue" 1 "llc_allocate" } } */

