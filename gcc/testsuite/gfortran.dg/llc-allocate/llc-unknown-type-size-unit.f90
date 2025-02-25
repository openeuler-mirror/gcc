! { dg-do compile { target { aarch64*-*-linux* } } }
! { dg-options "-c -O3 -march=armv8.2-a+sve -fllc-allocate -fdump-tree-llc_allocate-details-lineno --param filter-kernels=0 --param issue-topn=1 --param mem-access-ratio=5 --param mem-access-num=1" }

Module module_domain
    IMPLICIT NONE

    REAL, PARAMETER :: g = 9.8
    TYPE :: grid_type
        REAL, POINTER   :: phb(:,:,:), ph_2(:,:,:), p(:,:,:), pb(:,:,:)
        REAL, POINTER   :: fnm(:), fnp(:)
    END TYPE
END Module

SUBROUTINE calc_p8w(p8w, ix, iy, k_start, k_end)

   USE module_domain
   !USE module_model_constants

   IMPLICIT NONE


   !TYPE (domain), INTENT(IN) :: grid
   INTEGER, INTENT(IN) :: k_start, k_end, ix, iy
   REAL, DIMENSION(k_start:k_end), INTENT(OUT) :: p8w


   INTEGER :: k
   REAL    :: z0, z1, z2, w1, w2
   REAL, DIMENSION(k_start:k_end)   :: z_at_w
   REAL, DIMENSION(k_start:k_end-1) :: z
   TYPE (grid_type), POINTER :: grid


   DO k = k_start, k_end
      z_at_w(k) = (grid%phb(ix,k,iy)+grid%ph_2(ix,k,iy))/g
   END DO

   DO k = k_start, k_end-1
      z(k) = 0.5*(z_at_w(k) + z_at_w(k+1))
   END DO

   DO k = k_start+1, k_end-1
      p8w(k) = grid%fnm(k)*(grid%p(ix,k,iy)+grid%pb(ix,k,iy)) + &
               grid%fnp(k)*(grid%p(ix,k-1,iy)+grid%pb(ix,k-1,iy))
   END DO

   z0 = z_at_w(k_start)
   z1 = z(k_start)
   z2 = z(k_start+1)
   w1 = (z0 - z2)/(z1 - z2)
   w2 = 1. - w1
   p8w(k_start) = w1*(grid%p(ix,k_start,iy)+grid%pb(ix,k_start,iy)) + &
                  w2*(grid%p(ix,k_start+1,iy)+grid%pb(ix,k_start+1,iy))

END SUBROUTINE calc_p8w

! { dg-final { scan-tree-dump-times "runtime issue" 1 "llc_allocate" } }
! { dg-final { scan-tree-dump-times "static issue" 1 "llc_allocate" } }