! { dg-do compile }
! { dg-require-effective-target simdmath_header }
! { dg-options "-O3 -fsimdmath -ffp-model=strict" }
! Boards that switch vectorization off leave the declarations in place
! and emit no vector call, so the scan below cannot hold there.  Skip
! rather than gate on simdmath_vectorizes: these four are the only
! thing in the Fortran suite that fails when the pre-include breaks -
! measured, by deleting it - and that gate would turn the failure into
! UNSUPPORTED, which is how the C and C++ halves came to be blind.
! { dg-skip-if "needs vectorization" { *-*-* } { "-fno-tree-vectorize" "-march=*+nosimd*" } { "" } }
!
! Fortran reaches the vector variants through !GCC$ builtin attributes
! simd, which puts the attribute on the builtin declaration itself, so
! neither -fmath-errno nor -frounding-math gates it: the calls come out
! whatever the model asks for.  The C front end emits none here.  Saying
! "nothing was vectorized", which is what the C report says, would be
! false in the direction that reassures - so the Fortran report says what
! actually happens, and the call is still in the assembly to prove it.

program main
  real(8) :: a(1024), b(1024)
  integer :: i
  do i = 1, 1024
    a(i) = exp(b(i))
  end do
  if (a(1) < 0) stop 1
end program

! { dg-final { scan-assembler {_ZGVnN2v_exp} } }
! { dg-regexp {.*Warning: '-fsimdmath' vectorizes math calls here regardless of '-ffp-model=strict' \[-Wsimdmath\]} }
! { dg-regexp {.*note: the '!GCC\$ builtin' declarations it pre-includes attach the vector attribute directly, which '-frounding-math' does not gate} }
! { dg-regexp {.*note: a vector variant does not follow the rounding mode, so results may differ from the scalar calls} }
