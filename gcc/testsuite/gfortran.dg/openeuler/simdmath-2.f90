! { dg-do compile { target aarch64*-*-* } }
! { dg-options "-O3 -fsimdmath -msimdmath-vec64" }
! { dg-require-effective-target simdmath_vectorizes }
!
! With -msimdmath-vec64 the user asserts the vector math library provides
! 64-bit vector variants, so the mixed double/float loop (VF = 2) may call
! the V2SF clone _ZGVnN2v_sinf.
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
