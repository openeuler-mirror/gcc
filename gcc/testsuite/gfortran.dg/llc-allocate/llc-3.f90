! { dg-do compile { target { aarch64*-*-linux* } } }
! { dg-options "-O3 -march=armv8.2-a+sve -funroll-loops -ffast-math -static -fllc-allocate -fdump-tree-llc_allocate-details-lineno --param branch-prob-threshold=50 --param filter-mode=0" }

program main

  IMPLICIT NONE
  INTEGER :: ids,ide, jds,jde, kds,kde
  INTEGER,parameter :: ims=-4,kms=1,jms=-4
  INTEGER,parameter :: ime=210,kme=36,jme=192
  INTEGER :: its,ite, jts,jte, kts,kte
  INTEGER :: number_of_small_timesteps,rk_step, rk_order, step

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme) :: t_1, t_2, c2a, p, ph, pm1, al, alt


  REAL, DIMENSION(ims:ime, jms:jme) :: mu, muts

  REAL, DIMENSION(kms:kme) :: dnw, rdnw, znu

  REAL :: rdx,rdy
  REAL :: dts, t0, smdiv
  REAL :: random1,time_begin,time_end,total_time

  INTEGER :: i, j, k
  INTEGER :: i_start, i_end, j_start, j_end, k_start, k_end
  INTEGER :: i_endu, j_endv
  INTEGER :: interval=1
  INTEGER :: epoch,iter

  LOGICAL :: non_hydrostatic

  data ids, jds, kds, its, jts, kts /6*1/
  data ide, ite /2*205/
  data jde, jte /2*187/
  data kde, kte /2*36/

  number_of_small_timesteps = 1
  rk_step = 3
  rk_order = 1
  dts = 1.

  rdx = 1.
  rdy = 1.

  t0 = 0.
  smdiv = 1.
  step = 1
  non_hydrostatic = .true.

  call random_number(random1)
  interval = random1*100
  interval=1

  call random_seed(put=(/(i,i=1,10000,interval)/))

  call random_number(alt)
  call random_number(c2a)
  call random_number(ph)
  call random_number(pm1)
  call random_number(mu)
  call random_number(muts)
  call random_number(dnw)
  call random_number(rdnw)
  call random_number(znu)

  do iter=1,2
  call calc_p_rho( al, p, ph,                        &
                       alt, t_2, t_1, c2a, pm1,      &
                       mu, muts, znu, t0,            &
                       rdnw, dnw, smdiv,             &
                       non_hydrostatic, step,        &
                       ids, ide, jds, jde, kds, kde, &
                       ims, ime, jms, jme, kms, kme, &
                       its,ite, jts,jte, kts,kte    )

  enddo

end program


SUBROUTINE calc_p_rho( al, p, ph,                    &
                       alt, t_2, t_1, c2a, pm1,      &
                       mu, muts, znu, t0,            &
                       rdnw, dnw, smdiv,             &
                       non_hydrostatic, step,        &
                       ids, ide, jds, jde, kds, kde, &
                       ims, ime, jms, jme, kms, kme, &
                       its,ite, jts,jte, kts,kte    )

  IMPLICIT NONE  ! religion first
  !asb
! declarations for the stuff coming in

  INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde, kds,kde
  INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme, kms,kme
  INTEGER,      INTENT(IN   )    :: its,ite, jts,jte, kts,kte

  INTEGER,      INTENT(IN   )    :: step

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme),INTENT(  OUT) :: al,   &
                                                               p

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme),INTENT(IN   ) :: alt,   &
                                                              t_2,   &
                                                              t_1,   &
                                                              c2a

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme),INTENT(INOUT) :: ph, pm1

  REAL, DIMENSION(ims:ime, jms:jme)         , INTENT(IN   ) :: mu,   &
                                                               muts

  REAL, DIMENSION(kms:kme)         , INTENT(IN   ) :: dnw,  &
                                                      rdnw, &
                                                      znu

  REAL,                                       INTENT(IN   ) :: t0, smdiv

  LOGICAL, INTENT(IN   )  :: non_hydrostatic

! local variables

  INTEGER :: i, j, k
  INTEGER :: i_start, i_end, j_start, j_end, k_start, k_end
  REAL    :: ptmp

   i_start = its
   i_end   = min(ite,ide-1)
   j_start = jts
   j_end   = min(jte,jde-1)
   k_start = kts
   k_end = min(kte,kde-1)

   IF (non_hydrostatic) THEN
     DO j=j_start, j_end
     DO k=k_start, k_end
     DO i=i_start, i_end

!  al computation is all dry, so ok with moisture

      al(i,k,j)=-1./muts(i,j)*(alt(i,k,j)*mu(i,j)  &
             +rdnw(k)*(ph(i,k+1,j)-ph(i,k,j)))

!  this is temporally linearized p, no moisture correction needed

      p(i,k,j)=c2a(i,k,j)*(alt(i,k,j)*(t_2(i,k,j)-mu(i,j)*t_1(i,k,j))  &
                       /(muts(i,j)*(t0+t_1(i,k,j)))-al (i,k,j))

     ENDDO
     ENDDO
     ENDDO

   ELSE  ! hydrostatic calculation

       DO j=j_start, j_end
       DO k=k_start, k_end
       DO i=i_start, i_end
         p(i,k,j)=mu(i,j)*znu(k)
         al(i,k,j)=alt(i,k,j)*(t_2(i,k,j)-mu(i,j)*t_1(i,k,j))            &
                      /(muts(i,j)*(t0+t_1(i,k,j)))-p(i,k,j)/c2a(i,k,j)
         ph(i,k+1,j)=ph(i,k,j)-dnw(k)*(muts(i,j)*al (i,k,j)              &
                          +mu(i,j)*alt(i,k,j))
       ENDDO
       ENDDO
       ENDDO

   END IF

!  divergence damping setup

     IF (step == 0) then   ! we're initializing small timesteps
       DO j=j_start, j_end
       DO k=k_start, k_end
       DO i=i_start, i_end
         pm1(i,k,j)=p(i,k,j)
       ENDDO
       ENDDO
       ENDDO
     ELSE                     ! we're in the small timesteps
       DO j=j_start, j_end    ! and adding div damping component
       DO k=k_start, k_end
       DO i=i_start, i_end
         ptmp = p(i,k,j)
         p(i,k,j) = p(i,k,j) + smdiv*(p(i,k,j)-pm1(i,k,j))
         pm1(i,k,j) = ptmp
       ENDDO
       ENDDO
       ENDDO
     END IF

END SUBROUTINE calc_p_rho

! { dg-final { scan-tree-dump-times "ref_count = (?:\[3-9\]|\[1-9\]\\d{1,}), ninsns = \[1-9\]\\d*, mem_to_insn_ratio = 0.\[2-9\]\\d*" 6 "llc_allocate" } }
! { dg-final { scan-tree-dump-times "Tracing succeeded" 46 "llc_allocate" } }
! { dg-final { scan-tree-dump-not   "Tracing failed" "llc_allocate" } }
! { dg-final { scan-tree-dump-times "\{ (?:\\d+\\(\\d+\\) ){1}\}" 1 "llc_allocate" } }
! { dg-final { scan-tree-dump-times "\{ (?:\\d+\\(\\d+\\) ){2}\}" 2 "llc_allocate" } }
! { dg-final { scan-tree-dump-times "\{ (?:\\d+\\(\\d+\\) ){4}\}" 1 "llc_allocate" } }
! { dg-final { scan-tree-dump-times "\\d\\tp\\t\\(0.000000, 3, 1, 0\\)" 1 "llc_allocate" } }
! { dg-final { scan-tree-dump-times "\\d\\tp\\t\\(0.000000, 3, 3, 0\\)" 1 "llc_allocate" } }
! { dg-final { scan-tree-dump-times "\\d\\tpm1\\t\\(0.000000, 3, 2, 0\\)" 1 "llc_allocate" } }
! { dg-final { scan-tree-dump-times "\\d\\tph\\t\\(0.000000, 3, 2, 0\\)" 2 "llc_allocate" } }
! { dg-final { scan-tree-dump-times "\\d\\tal\\t\\(0.000000, 3, 1, 0\\)" 2 "llc_allocate" } }
! { dg-final { scan-tree-dump-times "\\d\\talt\\t\\(0.000000, 3, 1, 0\\)" 2 "llc_allocate" } }
! { dg-final { scan-tree-dump-times "\\d\\tt_1\\t\\(0.000000, 3, 1, 0\\)" 1 "llc_allocate" } }
! { dg-final { scan-tree-dump-times "\\d\\tt_2\\t\\(0.000000, 3, 1, 0\\)" 1 "llc_allocate" } }
! { dg-final { scan-tree-dump-times "\\d\\tc2a\\t\\(0.000000, 3, 1, 0\\)" 2 "llc_allocate" } }
! { dg-final { scan-tree-dump-times "runtime issue" 0 "llc_allocate" } }
! { dg-final { scan-tree-dump-times "static issue" 2 "llc_allocate" } }
! { dg-final { scan-tree-dump-times "insert svprfd" 2 "llc_allocate" } }
! { dg-final { scan-tree-dump-times "cumul_size.*150960\\)" 0 "llc_allocate" } }
