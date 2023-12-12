! { dg-do compile { target { aarch64*-*-linux* } } }
! { dg-options "-O3 -march=armv8.2-a+sve -static -fllc-allocate -fdump-tree-llc_allocate-details-lineno" }

MODULE INPUT
    IMPLICIT NONE

    INTEGER, PARAMETER :: wp = 8, jpi = 25, jpj = 39, jpk = 31, kjpt = 2

    INTEGER :: kt = 1, jpkm1 = 30, jpjm1 = 38, fs_jpim1 = 24, fs_2 = 2
    REAL(wp), DIMENSION(jpi, jpj) :: e12t
    REAL(wp), DIMENSION(jpi, jpj, jpk) :: fse3t_n
    REAL(wp), DIMENSION(jpi, jpj, jpk, kjpt) :: pta

END MODULE INPUT

PROGRAM MAIN
    USE INPUT

    IMPLICIT NONE

    INTEGER :: EPOCH

! Initialize arrays

    e12t = 1
    fse3t_n = 1
    pta = 1
!

    DO EPOCH=1,2
        CALL tra_ldf_iso
    ENDDO

END PROGRAM MAIN

SUBROUTINE tra_ldf_iso
    USE INPUT

    IMPLICIT NONE
    !
    INTEGER :: ji, jj, jk, jn   ! dummy loop indices
    REAL(wp) :: zbtr, ztra            !   -      -
    REAL(wp), DIMENSION(jpi, jpj, jpk) :: ztfw

    DO jn = 1, kjpt
        ztfw(:, :, 1) = 0.e0; ztfw(:, :, jpk) = 0.e0

        DO jk = 1, jpkm1
            DO jj = 2, jpjm1
                DO ji = fs_2, fs_jpim1   ! vector opt.
                    zbtr = 1.0/(e12t(ji, jj)*fse3t_n(ji, jj, jk))
                    ztra = (ztfw(ji, jj, jk) - ztfw(ji, jj, jk + 1))*zbtr
                    pta(ji, jj, jk, jn) = pta(ji, jj, jk, jn) + ztra
                END DO
            END DO
        END DO
        !
    END DO
    !
END SUBROUTINE tra_ldf_iso

! { dg-final { scan-tree-dump-times "Traced variables at vectp_ztfw" 2 "llc_allocate" } }
! { dg-final { scan-tree-dump-times "Tracing unusual number or occurrences of base variables.  Choose ztfw." 2 "llc_allocate" } }
