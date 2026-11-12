! { dg-do compile }
! { dg-options "-O2 -ffp-model=strict -fdump-tree-original" }
!
! The exception-preserving models report every exception but trap only the
! ones that mean a real problem.  Underflow is a routine outcome in
! denormal-heavy numerical code, so it stays out of the trap set:
! GFC_FPE_INVALID|ZERO|OVERFLOW = 1|4|8 = 13.  Adding UNDERFLOW (16) would
! make it 29 and abort programs that are computing correctly.

program main
  real(8) :: x
  x = 0.0d0
  if (x > 1.0d0) stop 1
end program

! { dg-final { scan-tree-dump "_gfortran_set_fpe \\(13\\)" "original" } }
