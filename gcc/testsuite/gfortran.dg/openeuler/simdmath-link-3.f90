! { dg-do link { target aarch64*-*-* } }
! { dg-set-compiler-env-var OPENEULER_GCC_SIMDMATH_LINK "bogus" }
! { dg-options "-O3 -fsimdmath" }
!
! An unrecognized policy value must be rejected with a fatal driver
! error naming the variable - never spliced into the link line.
!GCC$ builtin (pow) attributes simd (notinbranch)

program main
  real(8) :: a(16), b(16)
  integer :: i
  do i = 1, 16
    a(i) = a(i) ** b(i)
  end do
  if (a(1) < 0) stop 1
end program

! { dg-error "OPENEULER_GCC_SIMDMATH_LINK" "" { target *-*-* } 0 }
! { dg-excess-errors "fatal error followup" }
