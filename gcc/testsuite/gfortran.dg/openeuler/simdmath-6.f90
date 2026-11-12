! { dg-do compile { target aarch64*-*-* } }
! { dg-require-effective-target simdmath_header }
! { dg-options "-O3 -fsimdmath" }
! { dg-require-effective-target simdmath_vectorizes }
!
! Every other Fortran test here writes its own !GCC$ builtin directive,
! which leaves the pre-include itself - the thing that makes -fsimdmath
! work for code that just calls sin - completely untested.  This one
! declares nothing and relies on simdmath_f.h being loaded.

subroutine f (a, n)
  integer :: n, i
  real(4) :: a(n)
  do i = 1, n
    a(i) = sin (a(i))
  end do
end subroutine

! { dg-final { scan-assembler {_ZGVnN4v_sinf} } }
