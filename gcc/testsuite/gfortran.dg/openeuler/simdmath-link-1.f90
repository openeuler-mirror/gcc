! { dg-do link { target aarch64*-*-* } }
! { dg-require-effective-target libmathlib }
! { dg-options "-O3 -fsimdmath" }
! { dg-require-effective-target simdmath_vectorizes }
!
! Default policy (OPENEULER_GCC_SIMDMATH_LINK unset = as-needed): the
! driver injects libmathlib after the user objects, so the vectorized
! pow reference (_ZGVnN2vv_pow, not provided by glibc libmvec as of
! 2.38) resolves without any manual -lmathlib.
! Requires the optimized-routines package (libmathlib) to be installed.
!GCC$ builtin (pow) attributes simd (notinbranch)

program main
  real(8) :: a(1024), b(1024)
  integer :: i
  call random_number (a)
  call random_number (b)
  do i = 1, 1024
    a(i) = a(i) ** b(i)
  end do
  if (a(1) < 0) stop 1
end program
