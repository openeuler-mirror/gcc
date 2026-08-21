! { dg-do run }
! { dg-options "-O0 -ffp-model=strict" }
!
! End-to-end form of the same rule: gradual underflow to a denormal is a
! correct result, and the strict model must not turn it into SIGFPE.
! -O0 keeps the division from being folded away at compile time.

program main
  real(8) :: x, y
  x = tiny (1.0d0)
  y = 1.0d10
  x = x / y
  if (x > 1.0d0) stop 1
end program
