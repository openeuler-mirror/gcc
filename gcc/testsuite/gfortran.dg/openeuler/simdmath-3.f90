! { dg-do compile { target aarch64*-*-* } }
! { dg-options "-O3 -fsimdmath -fno-simdmath" }
! { dg-require-effective-target simdmath_vectorizes }
!
! -fno-simdmath clears the feature flag itself (and nothing more): with
! the flag off, the declaration gate is inactive and upstream clone
! enumeration returns, so the mixed double/float loop may again
! reference the 64-bit V2SF variant.  This distinguishes "flag really
! off" from "flag stuck on".
!GCC$ builtin (sinf) attributes simd (notinbranch)

subroutine mixed (a, b, n)
  integer :: n, i
  real(8) :: a(n)
  real(4) :: b(n)
  do i = 1, n
    a(i) = a(i) + 1.0d0
    b(i) = sin (b(i))
  end do
end subroutine

! { dg-final { scan-assembler {_ZGVnN2v_sinf} } }
