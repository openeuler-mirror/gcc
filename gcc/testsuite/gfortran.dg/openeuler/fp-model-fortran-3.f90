! { dg-do compile }
! { dg-options "-O2 -ffp-model=strict -ffpe-trap=invalid -fdump-tree-original" }
!
! The model's trap set is a default, not an override: a program that says
! which exceptions it wants to trap keeps its own choice, in either order
! on the command line.  GFC_FPE_INVALID alone is 1; the model's own set
! would be 13.

program main
  real(8) :: x
  x = 0.0d0
  if (x > 1.0d0) stop 1
end program

! { dg-final { scan-tree-dump "_gfortran_set_fpe \\(1\\)" "original" } }
