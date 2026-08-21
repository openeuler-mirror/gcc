! { dg-do compile { target aarch64*-*-* } }
! { dg-options "-O3 -fsimdmath" }
! { dg-require-effective-target simdmath_vectorizes }
!
! Under -fsimdmath, math builtin declarations must not use 64-bit vector
! simd clones: the vector math library only provides 128-bit variants.
! A mixed double/float loop (VF = 2, float vectype V2SF) must therefore
! not reference _ZGVnN2v_sinf, while a pure float loop (VF = 4) may still
! use the 128-bit _ZGVnN4v_sinf variant.
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

subroutine pure_float (b, n)
  integer :: n, i
  real(4) :: b(n)
  do i = 1, n
    b(i) = sin (b(i))
  end do
end subroutine

! { dg-final { scan-assembler-not {_ZGVnN2v_sinf} } }
! { dg-final { scan-assembler {_ZGVnN4v_sinf} } }
