! { dg-do compile }
! { dg-require-effective-target simdmath_header }
! { dg-options "-O3 -fsimdmath -ffp-model=except" }
! Boards that switch vectorization off leave the declarations in place
! and emit no vector call, so the scan below cannot hold there.  Skip
! rather than gate on simdmath_vectorizes: these four are the only
! thing in the Fortran suite that fails when the pre-include breaks -
! measured, by deleting it - and that gate would turn the failure into
! UNSUPPORTED, which is how the C and C++ halves came to be blind.
! { dg-skip-if "needs vectorization" { *-*-* } { "-fno-tree-vectorize" "-march=*+nosimd*" } { "" } }
!
! except keeps errno, and Fortran has none: gfc_init_options sets it off
! and marks it front-end set, so the model cannot turn it on.  There is
! therefore nothing for the report to say here, and it says nothing -
! unlike C, where this same command line disables seven of the eleven
! functions.  The second half of what makes the two reports different.

program main
  real(8) :: a(1024), b(1024)
  integer :: i
  do i = 1, 1024
    a(i) = exp(b(i))
  end do
  if (a(1) < 0) stop 1
end program

! { dg-final { scan-assembler {_ZGVnN2v_exp} } }
