/* Decoy for simdmath-nostdinc-1.c: a simdmath.h whose only content is
   the header's own guard macro.  If the -fsimdmath pre-include runs
   despite -nostdinc, this is the copy it must find - the test supplies
   this directory with -isystem, which -nostdinc does not remove, while
   every other copy sits on the bracket chain, which it does - and the
   macro turns the test red.  */
#define __SIMDMATH_H__ 1
