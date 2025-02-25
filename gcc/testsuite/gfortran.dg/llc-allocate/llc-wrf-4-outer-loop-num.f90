! { dg-do compile { target { aarch64*-*-linux* } } }
! { dg-options "-O3 -march=armv8.2-a+sve -static -fllc-allocate -fdump-tree-llc_allocate-details-lineno  --param=branch-prob-threshold=50 --param=filter-kernels=0 --param=mem-access-num=2 --param=issue-topn=2  --param=force-issue=1 --param=outer-loop-nums=3" }
!include "module_small_step_em.F90"

Module add_type
  IMPLICIT NONE

  TYPE :: grid_config_rec_type
      LOGICAL :: open_xs
      LOGICAL :: open_ys
      LOGICAL :: open_xe
      LOGICAL :: open_ye
      LOGICAL :: symmetric_xs
      LOGICAL :: symmetric_xe
      LOGICAL :: symmetric_ys
      LOGICAL :: symmetric_ye
      LOGICAL :: polar
      LOGICAL :: nested
      LOGICAL :: periodic_x
      LOGICAL :: specified
  END TYPE
END Module

program main


!  include "module_small_step_em_modify.F90"

!  use module_small_step_em
!  use module_small_step_em_modify

  use add_type

  IMPLICIT NONE
  INTEGER :: ids,ide, jds,jde, kds,kde
  INTEGER,parameter :: ims=-4,kms=1,jms=-4
  INTEGER,parameter :: ime=210,kme=36,jme=192
  INTEGER :: its,ite, jts,jte, kts,kte
  INTEGER :: number_of_small_timesteps,rk_step, rk_order, step, spec_zone

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme, 1:8) :: llcRefresh
  REAL, DIMENSION(ims:ime, kms:kme, jms:jme) :: u, v, u_1, v_1, t_1, ww_1, ft!u, v, u_1, v_1, w_1, t_1, ww1, ww_1,ph_1, ft
  REAL, DIMENSION(ims:ime, kms:kme, jms:jme) :: u_save, v_save, w_save, t_save, ph_save,h_diabatic
  ! REAL, DIMENSION(ims:ime, kms:kme, jms:jme) :: u_2, v_2, w_2, t_2, ph_2
  ! REAL, DIMENSION(ims:ime, kms:kme, jms:jme) :: c2a, ww_save, cqw, cqu, cqv, alpha, gamma, a
  REAL, DIMENSION(ims:ime, kms:kme, jms:jme) :: ww!pb, p, ph, php, pm1, al, alt, ww, random_array
  ! REAL, DIMENSION(ims:ime, kms:kme, jms:jme) :: ru_tend, rv_tend
  REAL, DIMENSION(ims:ime, kms:kme, jms:jme) :: t, t_ave, uam, vam, wwam

  REAL, DIMENSION(ims:ime, jms:jme) :: mu_1,mu_2, mu
  REAL, DIMENSION(ims:ime, jms:jme) :: mub, muu, muv, mut,        &
                                       msfux, msfuy,              &
                                       msfvx, msfvx_inv, msfvy,   &
                                       msftx, msfty

  REAL, DIMENSION(ims:ime, jms:jme) :: muus, muvs, muts, mudf, muave
  REAL, DIMENSION(ims:ime, jms:jme) :: mu_save, mu_tend

  REAL, DIMENSION(kms:kme) :: rdn, rdnw,dnw, fnm, fnp, znu

  REAL :: rdx,rdy
  REAL :: dts, cf1, cf2, cf3, t0, emdiv, smdiv, epssm, g
  REAL :: random1,time_begin,time_end,total_time

  INTEGER :: i, j, k
  INTEGER :: i_start, i_end, j_start, j_end, k_start, k_end
  INTEGER :: i_endu, j_endv
  INTEGER :: interval=1
  INTEGER :: epoch

  LOGICAL :: non_hydrostatic, top_lid


  TYPE (grid_config_rec_type) :: config_flags
  config_flags%open_xs = .true.
  config_flags%open_ys = .true.
  config_flags%open_xe = .true.
  config_flags%open_ye = .true.
  config_flags%symmetric_xs = .true.
  config_flags%symmetric_xe = .true.
  config_flags%symmetric_ys = .true.
  config_flags%symmetric_ye = .true.
  config_flags%polar = .true.
  config_flags%nested = .true.
  config_flags%periodic_x = .true.
  config_flags%specified = .true.

  data ids, jds, kds, its, jts, kts /6*1/
  data ide, ite /2*205/
  data jde, jte /2*187/
  data kde, kte /2*98/

  number_of_small_timesteps = 1
  rk_step = 1
  rk_order = 1
  dts = 1.
  epssm = 1.
  g = 1.

  rdx = 1.
  rdy = 1.
  dts = 1.
  cf1 = 1.
  cf2 = 1.
  cf3 = 1.

  t0 = 0.
  smdiv = 1.
  emdiv = 1.
  step = 1
  spec_zone = 1

  non_hydrostatic = .true.
  top_lid = .true.

  interval=1


  total_time=0

  call random_seed(put=(/(i,i=1,10000,interval)/))

  call random_number(u)
  call random_number(v)
  call random_number(u_1)
  call random_number(v_1)
  call random_number(t_1)
  call random_number(ft)

  call random_number(ww)
  call random_number(ww_1)
  call random_number(t)
  call random_number(t_ave)
  call random_number(uam)
  call random_number(vam)
  call random_number(wwam)

  call random_number(muu)
  call random_number(muv)
  call random_number(mut)
  call random_number(msfux)
  call random_number(msfuy)
  call random_number(msfvx)
  call random_number(msfvx_inv)
  call random_number(msfvy)
  call random_number(msftx)
  call random_number(msfty)
  call random_number(mu_tend)

  call random_number(muave)
  call random_number(muts)
  call random_number(mudf)
  call random_number(mu)

  call random_number(fnm)
  call random_number(fnp)
  call random_number(dnw)
  call random_number(rdnw)

  DO j=jms, jme
  DO k=kms, kme
  DO i=ims, ime

    llcRefresh(i,k,j,1)=i+k+j+7

  ENDDO
  ENDDO
  ENDDO

  do epoch = 1,2
  call advance_mu_t_fortran_plu( ww, ww_1, u, u_1, v, v_1,            &
                         mu, mut, muave, muts, muu, muv,      &
                         mudf, uam, vam, wwam, t, t_1,        &
                         t_ave, ft, mu_tend,                  &
                         rdx, rdy, dts, epssm,                &
                         dnw, fnm, fnp, rdnw,                 &
                         msfux, msfuy, msfvx, msfvx_inv,      &
                         msfvy, msftx, msfty,                 &
                         step, config_flags,                  &
                         ids, ide, jds, jde, kds, kde,        &
                         ims, ime, jms, jme, kms, kme,        &
                         its, ite, jts, jte, kts, kte        )
  enddo
end program



SUBROUTINE advance_mu_t_fortran_plu( ww, ww_1, u, u_1, v, v_1,            &
        mu, mut, muave, muts, muu, muv,      &
        mudf, uam, vam, wwam, t, t_1,        &
        t_ave, ft, mu_tend,                  &
        rdx, rdy, dts, epssm,                &
        dnw, fnm, fnp, rdnw,                 &
        msfux, msfuy, msfvx, msfvx_inv,      &
        msfvy, msftx, msfty,                 &
        step, config_flags,                  &
        ids, ide, jds, jde, kds, kde,        &
        ims, ime, jms, jme, kms, kme,        &
        its, ite, jts, jte, kts, kte        )
  use add_type

  IMPLICIT NONE  ! religion first

  ! stuff coming in

  TYPE(grid_config_rec_type), INTENT(IN   ) :: config_flags
  INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde, kds,kde
  INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme, kms,kme
  INTEGER,      INTENT(IN   )    :: its,ite, jts,jte, kts,kte

  INTEGER,      INTENT(IN   )    :: step

  REAL, DIMENSION( ims:ime , kms:kme, jms:jme ),   &
          INTENT(IN   ) ::                       &
          u,   &
          v,   &
          u_1, &
          v_1, &
          t_1, &
          ft

  REAL, DIMENSION( ims:ime , kms:kme, jms:jme ),      &
          INTENT(INOUT) ::                          &
          ww,     &
          ww_1,   &
          t,      &
          t_ave,  &
          uam,    &
          vam,    &
          wwam

  REAL, DIMENSION( ims:ime , jms:jme ),    INTENT(IN   ) :: muu,  &
          muv,  &
          mut,  &
          msfux,&
          msfuy,&
          msfvx,&
          msfvx_inv,&
          msfvy,&
          msftx,&
          msfty,&
          mu_tend

  REAL, DIMENSION( ims:ime , jms:jme ),    INTENT( INOUT) :: muave, &
          muts,  &
          mudf

  REAL, DIMENSION( ims:ime , jms:jme ),    INTENT(INOUT) :: mu

  REAL, DIMENSION( kms:kme ),              INTENT(IN   ) :: fnm,    &
          fnp,    &
          dnw,    &
          rdnw


  REAL,                                    INTENT(IN   ) :: rdx,    &
          rdy,    &
          dts,    &
          epssm

  REAL, DIMENSION (its:ite, kts:kte) :: wdtn, dvdxi
  REAL, DIMENSION (its:ite) :: dmdt

  INTEGER :: i,j,k, i_start, i_end, j_start, j_end, k_start, k_end
  INTEGER :: i_endu, j_endv
  REAL    :: acc

  INTEGER :: ubv, lbv, t1, t2, t3, t4, ceild, floord

  ceild(t1, t2) = ceiling(REAL(t1)/REAL(t2))
  floord(t1, t2) = floor(REAL(t1)/REAL(t2))
  i_start = its
  i_end   = min(ite,ide-1)
  j_start = jts
  j_end   = min(jte,jde-1)
  k_start = kts
  k_end   = kte-1
  IF ( .NOT. config_flags%periodic_x )THEN
    IF ( config_flags%specified .or. config_flags%nested ) then
      i_start = max(its,ids+1)
      i_end   = min(ite,ide-2)
    ENDIF
  ENDIF
  IF ( config_flags%specified .or. config_flags%nested ) then
    j_start = max(jts,jds+1)
    j_end   = min(jte,jde-2)
  ENDIF

  i_endu = ite
  j_endv = jte

  DO j = j_start, j_end

    DO i=i_start, i_end
      dmdt(i) = 0.
    ENDDO

    DO k=k_start, k_end
      DO i=i_start, i_end
        dvdxi(i,k) = msftx(i,j)*msfty(i,j)*(      &
                rdy*((v(i,k,j+1)+muv(i,j+1)*v_1(i,k,j+1)*msfvx_inv(i,j+1))  &
                        -(v(i,k,j  )+muv(i,j  )*v_1(i,k,j)*msfvx_inv(i,j  ))) &
                        +rdx*((u(i+1,k,j)+muu(i+1,j)*u_1(i+1,k,j)/msfuy(i+1,j))      &
                        -(u(i,k,j  )+muu(i  ,j)*u_1(i,k,j  )/msfuy(i,j)) ))
        dmdt(i)    = dmdt(i) + dnw(k)*dvdxi(i,k)
      ENDDO
    ENDDO
    DO i=i_start, i_end
      muave(i,j) = mu(i,j)
      mu(i,j) = mu(i,j)+dts*(dmdt(i)+mu_tend(i,j))
      mudf(i,j) = (dmdt(i)+mu_tend(i,j)) ! save tendency for div dampfilter
      muts(i,j) = mut(i,j)+mu(i,j)
      muave(i,j) =.5*((1.+epssm)*mu(i,j)+(1.-epssm)*muave(i,j))
    ENDDO
  ENDDO
END SUBROUTINE advance_mu_t_fortran_plu

! { dg-final { scan-tree-dump "issue_llc_hint" "llc_allocate" } }
! { dg-final { scan-tree-dump-times "analyze_nested_kernels" 2 "llc_allocate" } }
! { dg-final { scan-tree-dump "Stop tracing the outer loop depth" "llc_allocate" } }