! { dg-do link { target aarch64*-*-* } }
! { dg-set-compiler-env-var OPENEULER_GCC_SIMDMATH_LINK "none" }
! { dg-options "-O3 -fsimdmath" }
! { dg-require-effective-target simdmath_vectorizes }
! Pin the reason, not just the failure: a bare dg-excess-errors passes on
! any link error at all.  dg-error is no use here because ld prefixes the
! message with `file:(.text+0x90):', which is not a line number, so the
! message is attributed to no line; dg-regexp matches the output directly.
! { dg-regexp "undefined reference to ._ZGVnN2vv_pow." }
! { dg-excess-errors "link failure followup" }
!
! Under the none policy no vector math library is linked, so the call the
! vectorizer creates has nothing to resolve against - glibc's libmvec has
! no pow - and the link fails.  That failure is the point of the test.
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
