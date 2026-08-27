! { dg-do compile { target aarch64*-*-* } }
! { dg-options "-O3 -fsimdmath" }
! { dg-require-effective-target simdmath_vectorizes }
! The vector variants asserted below come from the program's own
! !$omp declare simd, which -fno-openmp-simd leaves dormant.  The
! simdmath_vectorizes probe cannot speak for this: it measures a**b,
! which reaches the variants through !GCC$ builtin and is not gated by
! that option, so it answers yes on a board where these cannot hold.
! { dg-skip-if "needs omp declare simd active" { *-*-* } { "-fno-openmp-simd" } { "" } }
!
! -fsimdmath implies -fopenmp-simd, without which declare-simd directives
! are ignored and nothing is ever cloned.  The Fortran math path would not
! notice - it declares its variants with !GCC$ builtin - but the C and C++
! path is entirely OpenMP pragmas, so dropping the implication turns the
! whole feature into a silent no-op there.

function myd (x)
  real(8) :: myd, x
  !$omp declare simd(myd) notinbranch
  myd = x * 2.0d0 + 1.0d0
end function

! { dg-final { scan-assembler {_ZGVnN2v_myd} } }
