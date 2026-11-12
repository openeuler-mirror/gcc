! { dg-do compile }
! { dg-options "-O2 -ffp-model=strict -ffpe-summary=none -fdump-tree-original" }
!
! The summary set is a default of the model in the same way the trap set
! is: a program that says which exceptions it wants reported keeps its
! own choice.  The summary mask is the last element of the options array
! the runtime is handed: the model's `all' is 63, `none' is 0.

program main
  real(8) :: x
  x = 0.0d0
  if (x > 1.0d0) stop 1
end program

! { dg-final { scan-tree-dump "1, 1, 0, 0" "original" } }
