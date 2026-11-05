! { dg-do compile }
! { dg-require-effective-target simdmath_header }
! { dg-options "-O3 -fsimdmath -fmath-errno" }
! Boards that switch vectorization off leave the declarations in place
! and emit no vector call, so the scan below cannot hold there.  Skip
! rather than gate on simdmath_vectorizes: these four are the only
! thing in the Fortran suite that fails when the pre-include breaks -
! measured, by deleting it - and that gate would turn the failure into
! UNSUPPORTED, which is how the C and C++ halves came to be blind.
! { dg-skip-if "needs vectorization" { *-*-* } { "-fno-tree-vectorize" "-march=*+nosimd*" } { "" } }
!
! The errno dimension is inert in Fortran and the report says nothing
! about it.  gfc_init_options fixes flag_errno_math off and marks it
! front-end set; Fortran intrinsics do not set errno, and no measurement
! showed an explicit -fmath-errno changing what gfortran generates.  So
! -fsimdmath is not taking away anything the user was getting, and a
! report claiming otherwise would be the same kind of false statement
! that removed the C report from this front end in the first place.
!
! simdmath-unconstrained-1.f90 has the case that is real: the rounding
! mode, which a vector variant genuinely does not follow.

program main
  real(8) :: a(1024), b(1024)
  integer :: i
  do i = 1, 1024
    a(i) = exp(b(i))
  end do
  if (a(1) < 0) stop 1
end program

! { dg-final { scan-assembler {_ZGVnN2v_exp} } }
