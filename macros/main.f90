
#include "macros.h"

program test_mac_vs_sub

   use matrix3
   use matrix

   implicit none

   integer, parameter :: nvect = 1000000
   integer, parameter :: N = 1000

   real(8), allocatable :: values(:,:), v_sub(:), v_mac(:)
   real(8)              :: v(3),M(3,3),MT(3,3)
   real(8)              :: ML(3,3),MR(3,3),MM(3,3)
   integer(8)           :: tin,tout, t_sub,t_mac
   real(8)              :: cin,cout, c_sub,c_mac
   integer              :: i,j


   allocate(values(3,nvect),v_sub(nvect),v_mac(nvect))
   do i=1,nvect
      call random_number(values(1,i))
      call random_number(values(2,i))
      call random_number(values(3,i))
   end do

   write(*,*)
   write(*,*) "======================================================"
   write(*,*) "Testing NORM"
   write(*,*) "======================================================"

   call system_clock(tin)
   call cpu_time(cin)
   do j=1,100
      do i=1,nvect
         v = values(:,i)
         v_mac(i) = MAC_VECTOR3_NORM(v)
      end do
   end do
   call cpu_time(cout)
   call system_clock(tout)
   t_mac = tout-tin
   c_mac = cout-cin

   write(*,*)
   write(*,*) 'Module matrix3'
   call system_clock(tin)
   call cpu_time(cin)
   do j=1,100
      do i=1,nvect
         v = values(:,i)
         !DIR$ FORCEINLINE
         v_sub(i) = MOD_VECTOR3_norm(v)
      end do
   end do
   call cpu_time(cout)
   call system_clock(tout)
   t_sub = tout-tin
   c_sub = cout-cin

   write(*,'(a,i10,a,i10,a,f10.3,a)')       't-sub =',t_sub,', t-mac = ',t_mac,', 100% +',100*dble(t_sub-t_mac)/dble(t_mac),'%'
   write(*,'(a,f10.7,a,f10.7,a,f10.3,a)')   'c-sub =',c_sub,', c-mac = ',c_mac,', 100% +',100*dble(c_sub-c_mac)/dble(c_mac),'%'

   write(*,*)
   write(*,*) 'Contained matrix3'
   call system_clock(tin)
   call cpu_time(cin)
   do j=1,100
      do i=1,nvect
         v = values(:,i)
         !DIR$ FORCEINLINE
         v_sub(i) = CONT_VECTOR3_norm(v)
      end do
   end do
   call cpu_time(cout)
   call system_clock(tout)
   t_sub = tout-tin
   c_sub = cout-cin

   write(*,'(a,i10,a,i10,a,f10.3,a)')       't-sub =',t_sub,', t-mac = ',t_mac,', 100% +',100*dble(t_sub-t_mac)/dble(t_mac),'%'
   write(*,'(a,f10.7,a,f10.7,a,f10.3,a)')   'c-sub =',c_sub,', c-mac = ',c_mac,', 100% +',100*dble(c_sub-c_mac)/dble(c_mac),'%'

   write(*,*)
   write(*,*) 'Generalistic matrix module'
   call system_clock(tin)
   call cpu_time(cin)
   do j=1,100
      do i=1,nvect
         v = values(:,i)
         !DIR$ FORCEINLINE
         v_sub(i) = MOD_VECTOR_norm(v)
      end do
   end do
   call cpu_time(cout)
   call system_clock(tout)
   t_sub = tout-tin
   c_sub = cout-cin

   write(*,'(a,i10,a,i10,a,f10.3,a)')       't-sub =',t_sub,', t-mac = ',t_mac,', 100% +',100*dble(t_sub-t_mac)/dble(t_mac),'%'
   write(*,'(a,f10.7,a,f10.7,a,f10.3,a)')   'c-sub =',c_sub,', c-mac = ',c_mac,', 100% +',100*dble(c_sub-c_mac)/dble(c_mac),'%'

   write(*,*)
   write(*,*) 'Intrinsic'
   call system_clock(tin)
   call cpu_time(cin)
   do j=1,100
      do i=1,nvect
         v = values(:,i)
         v_sub(i) = norm2(v)
      end do
   end do
   call cpu_time(cout)
   call system_clock(tout)
   t_sub = tout-tin
   c_sub = cout-cin

   write(*,'(a,i10,a,i10,a,f10.3,a)')       't-sub =',t_sub,', t-mac = ',t_mac,', 100% +',100*dble(t_sub-t_mac)/dble(t_mac),'%'
   write(*,'(a,f10.7,a,f10.7,a,f10.3,a)')   'c-sub =',c_sub,', c-mac = ',c_mac,', 100% +',100*dble(c_sub-c_mac)/dble(c_mac),'%'

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   write(*,*)
   write(*,*) "======================================================"
   write(*,*) "Testing TRACE"
   write(*,*) "======================================================"

   call system_clock(tin)
   call cpu_time(cin)
   do j=1,100
      do i=1,nvect
         M(1:3,1) = values(:,i)
         M(1:3,2) = values(:,i)
         M(1:3,3) = values(:,i)
         v_mac(i) = MAC_MATRIX33_TRACE(M)
      end do
   end do
   call cpu_time(cout)
   call system_clock(tout)
   t_mac = tout-tin
   c_mac = cout-cin

   write(*,*)
   write(*,*) 'Module matrix3'
   call system_clock(tin)
   call cpu_time(cin)
   do j=1,100
      do i=1,nvect
         M(1:3,1) = values(:,i)
         M(1:3,2) = values(:,i)
         M(1:3,3) = values(:,i)
         !DIR$ FORCEINLINE
         v_sub(i) = MOD_MATRIX33_trace(M)
      end do
   end do
   call cpu_time(cout)
   call system_clock(tout)
   t_sub = tout-tin
   c_sub = cout-cin

   write(*,'(a,i10,a,i10,a,f10.3,a)')       't-sub =',t_sub,', t-mac = ',t_mac,', 100% +',100*dble(t_sub-t_mac)/dble(t_mac),'%'
   write(*,'(a,f10.7,a,f10.7,a,f10.3,a)')   'c-sub =',c_sub,', c-mac = ',c_mac,', 100% +',100*dble(c_sub-c_mac)/dble(c_mac),'%'

   write(*,*)
   write(*,*) 'Contained matrix3'
   call system_clock(tin)
   call cpu_time(cin)
   do j=1,100
      do i=1,nvect
         M(1:3,1) = values(:,i)
         M(1:3,2) = values(:,i)
         M(1:3,3) = values(:,i)
         !DIR$ FORCEINLINE
         v_sub(i) = CONT_MATRIX33_trace(M)
      end do
   end do
   call cpu_time(cout)
   call system_clock(tout)
   t_sub = tout-tin
   c_sub = cout-cin

   write(*,'(a,i10,a,i10,a,f10.3,a)')       't-sub =',t_sub,', t-mac = ',t_mac,', 100% +',100*dble(t_sub-t_mac)/dble(t_mac),'%'
   write(*,'(a,f10.7,a,f10.7,a,f10.3,a)')   'c-sub =',c_sub,', c-mac = ',c_mac,', 100% +',100*dble(c_sub-c_mac)/dble(c_mac),'%'

   write(*,*)
   write(*,*) 'Generalistic matrix module'
   call system_clock(tin)
   call cpu_time(cin)
   do j=1,100
      do i=1,nvect
         M(1:3,1) = values(:,i)
         M(1:3,2) = values(:,i)
         M(1:3,3) = values(:,i)
         !DIR$ FORCEINLINE
         v_sub(i) = MOD_MATRIX_trace(M)
      end do
   end do
   call cpu_time(cout)
   call system_clock(tout)
   t_sub = tout-tin
   c_sub = cout-cin

   write(*,'(a,i10,a,i10,a,f10.3,a)')       't-sub =',t_sub,', t-mac = ',t_mac,', 100% +',100*dble(t_sub-t_mac)/dble(t_mac),'%'
   write(*,'(a,f10.7,a,f10.7,a,f10.3,a)')   'c-sub =',c_sub,', c-mac = ',c_mac,', 100% +',100*dble(c_sub-c_mac)/dble(c_mac),'%'

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   write(*,*)
   write(*,*) "======================================================"
   write(*,*) "Testing TRANSPOSE"
   write(*,*) "======================================================"

   call system_clock(tin)
   call cpu_time(cin)
   do j=1,100
      do i=1,nvect
         M(1:3,1) = values(:,i)
         M(1:3,2) = values(:,i)
         M(1:3,3) = values(:,i)
         MAC_MATRIX33_TRANSPOSE(M, MT)
      end do
   end do
   call cpu_time(cout)
   call system_clock(tout)
   t_mac = tout-tin
   c_mac = cout-cin

   write(*,*)
   write(*,*) 'Module matrix3'
   call system_clock(tin)
   call cpu_time(cin)
   do j=1,100
      do i=1,nvect
         M(1:3,1) = values(:,i)
         M(1:3,2) = values(:,i)
         M(1:3,3) = values(:,i)
         !DIR$ FORCEINLINE
         call MOD_MATRIX33_transpose(M, MT)
      end do
   end do
   call cpu_time(cout)
   call system_clock(tout)
   t_sub = tout-tin
   c_sub = cout-cin

   write(*,'(a,i10,a,i10,a,f10.3,a)')       't-sub =',t_sub,', t-mac = ',t_mac,', 100% +',100*dble(t_sub-t_mac)/dble(t_mac),'%'
   write(*,'(a,f10.7,a,f10.7,a,f10.3,a)')   'c-sub =',c_sub,', c-mac = ',c_mac,', 100% +',100*dble(c_sub-c_mac)/dble(c_mac),'%'

   write(*,*)
   write(*,*) 'Contained matrix3'
   call system_clock(tin)
   call cpu_time(cin)
   do j=1,100
      do i=1,nvect
         M(1:3,1) = values(:,i)
         M(1:3,2) = values(:,i)
         M(1:3,3) = values(:,i)
         !DIR$ FORCEINLINE
         call CONT_MATRIX33_transpose(M, MT)
      end do
   end do
   call cpu_time(cout)
   call system_clock(tout)
   t_sub = tout-tin
   c_sub = cout-cin

   write(*,'(a,i10,a,i10,a,f10.3,a)')       't-sub =',t_sub,', t-mac = ',t_mac,', 100% +',100*dble(t_sub-t_mac)/dble(t_mac),'%'
   write(*,'(a,f10.7,a,f10.7,a,f10.3,a)')   'c-sub =',c_sub,', c-mac = ',c_mac,', 100% +',100*dble(c_sub-c_mac)/dble(c_mac),'%'

   write(*,*)
   write(*,*) 'Generalistic matrix module'
   call system_clock(tin)
   call cpu_time(cin)
   do j=1,100
      do i=1,nvect
         M(1:3,1) = values(:,i)
         M(1:3,2) = values(:,i)
         M(1:3,3) = values(:,i)
         !DIR$ FORCEINLINE
         call MOD_MATRIX_transpose(M, MT)
      end do
   end do
   call cpu_time(cout)
   call system_clock(tout)
   t_sub = tout-tin
   c_sub = cout-cin

   write(*,'(a,i10,a,i10,a,f10.3,a)')       't-sub =',t_sub,', t-mac = ',t_mac,', 100% +',100*dble(t_sub-t_mac)/dble(t_mac),'%'
   write(*,'(a,f10.7,a,f10.7,a,f10.3,a)')   'c-sub =',c_sub,', c-mac = ',c_mac,', 100% +',100*dble(c_sub-c_mac)/dble(c_mac),'%'

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   write(*,*)
   write(*,*) 'Intrinsic'
   call system_clock(tin)
   call cpu_time(cin)
   do j=1,100
      do i=1,nvect
         M(1:3,1) = values(:,i)
         M(1:3,2) = values(:,i)
         M(1:3,3) = values(:,i)
         MT = transpose(M)
      end do
   end do
   call cpu_time(cout)
   call system_clock(tout)
   t_sub = tout-tin
   c_sub = cout-cin

   write(*,'(a,i10,a,i10,a,f10.3,a)')       't-sub =',t_sub,', t-mac = ',t_mac,', 100% +',100*dble(t_sub-t_mac)/dble(t_mac),'%'
   write(*,'(a,f10.7,a,f10.7,a,f10.3,a)')   'c-sub =',c_sub,', c-mac = ',c_mac,', 100% +',100*dble(c_sub-c_mac)/dble(c_mac),'%'

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   write(*,*)
   write(*,*) "======================================================"
   write(*,*) "Testing MATRIX PRODUCT"
   write(*,*) "======================================================"

   call system_clock(tin)
   call cpu_time(cin)
   do j=1,20
      do i=1,nvect
         ML(1:3,1) = values(:,i)
         ML(1:3,2) = values(:,i)
         ML(1:3,3) = values(:,i)
         MR = ML
         MAC_MATRIX33_PROD(ML,MR,MM)
      end do
   end do
   call cpu_time(cout)
   call system_clock(tout)
   t_mac = tout-tin
   c_mac = cout-cin

   write(*,*)
   write(*,*) 'Module matrix3'
   call system_clock(tin)
   call cpu_time(cin)
   do j=1,20
      do i=1,nvect
         ML(1:3,1) = values(:,i)
         ML(1:3,2) = values(:,i)
         ML(1:3,3) = values(:,i)
         MR = ML
         !DIR$ FORCEINLINE
         call MOD_MATRIX33_prod(ML,MR,MM)
      end do
   end do
   call cpu_time(cout)
   call system_clock(tout)
   t_sub = tout-tin
   c_sub = cout-cin

   write(*,'(a,i10,a,i10,a,f10.3,a)')       't-sub =',t_sub,', t-mac = ',t_mac,', 100% +',100*dble(t_sub-t_mac)/dble(t_mac),'%'
   write(*,'(a,f10.7,a,f10.7,a,f10.3,a)')   'c-sub =',c_sub,', c-mac = ',c_mac,', 100% +',100*dble(c_sub-c_mac)/dble(c_mac),'%'

#ifdef __INTEL_COMPILER
   write(*,*)
   write(*,*) 'Module matrix3 (no forceinline)'
   call system_clock(tin)
   call cpu_time(cin)
   do j=1,20
      do i=1,nvect
         ML(1:3,1) = values(:,i)
         ML(1:3,2) = values(:,i)
         ML(1:3,3) = values(:,i)
         MR = ML
         call MOD_MATRIX33_prod(ML,MR,MM)
      end do
   end do
   call cpu_time(cout)
   call system_clock(tout)
   t_sub = tout-tin
   c_sub = cout-cin

   write(*,'(a,i10,a,i10,a,f10.3,a)')       't-sub =',t_sub,', t-mac = ',t_mac,', 100% +',100*dble(t_sub-t_mac)/dble(t_mac),'%'
   write(*,'(a,f10.7,a,f10.7,a,f10.3,a)')   'c-sub =',c_sub,', c-mac = ',c_mac,', 100% +',100*dble(c_sub-c_mac)/dble(c_mac),'%'
#endif

   write(*,*)
   write(*,*) 'Contained matrix3'
   call system_clock(tin)
   call cpu_time(cin)
   do j=1,20
      do i=1,nvect
         ML(1:3,1) = values(:,i)
         ML(1:3,2) = values(:,i)
         ML(1:3,3) = values(:,i)
         MR = ML
         !DIR$ FORCEINLINE
         call CONT_MATRIX33_prod(ML,MR,MM)
      end do
   end do
   call cpu_time(cout)
   call system_clock(tout)
   t_sub = tout-tin
   c_sub = cout-cin

   write(*,'(a,i10,a,i10,a,f10.3,a)')       't-sub =',t_sub,', t-mac = ',t_mac,', 100% +',100*dble(t_sub-t_mac)/dble(t_mac),'%'
   write(*,'(a,f10.7,a,f10.7,a,f10.3,a)')   'c-sub =',c_sub,', c-mac = ',c_mac,', 100% +',100*dble(c_sub-c_mac)/dble(c_mac),'%'

   write(*,*)
   write(*,*) 'Generalistic matrix module'
   call system_clock(tin)
   call cpu_time(cin)
   do j=1,20
      do i=1,nvect
         ML(1:3,1) = values(:,i)
         ML(1:3,2) = values(:,i)
         ML(1:3,3) = values(:,i)
         MR = ML
         !DIR$ FORCEINLINE
         call MOD_MATRIX_prod(ML,MR,MM)
      end do
   end do
   call cpu_time(cout)
   call system_clock(tout)
   t_sub = tout-tin
   c_sub = cout-cin

   write(*,'(a,i10,a,i10,a,f10.3,a)')       't-sub =',t_sub,', t-mac = ',t_mac,', 100% +',100*dble(t_sub-t_mac)/dble(t_mac),'%'
   write(*,'(a,f10.7,a,f10.7,a,f10.3,a)')   'c-sub =',c_sub,', c-mac = ',c_mac,', 100% +',100*dble(c_sub-c_mac)/dble(c_mac),'%'

#ifdef __INTEL_COMPILER
   write(*,*)
   write(*,*) 'Generalistic matrix module (no forceinline)'
   call system_clock(tin)
   call cpu_time(cin)
   do j=1,20
      do i=1,nvect
         ML(1:3,1) = values(:,i)
         ML(1:3,2) = values(:,i)
         ML(1:3,3) = values(:,i)
         MR = ML
         !DIR$ FORCEINLINE
         call MOD_MATRIX_prod(ML,MR,MM)
      end do
   end do
   call cpu_time(cout)
   call system_clock(tout)
   t_sub = tout-tin
   c_sub = cout-cin

   write(*,'(a,i10,a,i10,a,f10.3,a)')       't-sub =',t_sub,', t-mac = ',t_mac,', 100% +',100*dble(t_sub-t_mac)/dble(t_mac),'%'
   write(*,'(a,f10.7,a,f10.7,a,f10.3,a)')   'c-sub =',c_sub,', c-mac = ',c_mac,', 100% +',100*dble(c_sub-c_mac)/dble(c_mac),'%'
#endif

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   write(*,*)
   write(*,*) 'intrinsic matmul'
   call system_clock(tin)
   call cpu_time(cin)
   do j=1,20
      do i=1,nvect
         ML(1:3,1) = values(:,i)
         ML(1:3,2) = values(:,i)
         ML(1:3,3) = values(:,i)
         MR = ML
         MM = matmul(ML,MR)
      end do
   end do
   call cpu_time(cout)
   call system_clock(tout)
   t_sub = tout-tin
   c_sub = cout-cin

   write(*,'(a,i10,a,i10,a,f10.3,a)')       't-sub =',t_sub,', t-mac = ',t_mac,', 100% +',100*dble(t_sub-t_mac)/dble(t_mac),'%'
   write(*,'(a,f10.7,a,f10.7,a,f10.3,a)')   'c-sub =',c_sub,', c-mac = ',c_mac,', 100% +',100*dble(c_sub-c_mac)/dble(c_mac),'%'

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   write(*,*)
   write(*,*) "======================================================"
   write(*,*) "Testing DETERMINANT"
   write(*,*) "======================================================"

   call system_clock(tin)
   call cpu_time(cin)
   do j=1,100
      do i=1,nvect
         M(1:3,1) = values(:,i)
         M(1:3,2) = values(:,i)
         M(1:3,3) = values(:,i)
         v_mac(i) = MAC_MATRIX33_DET(M)
      end do
   end do
   call cpu_time(cout)
   call system_clock(tout)
   t_mac = tout-tin
   c_mac = cout-cin

   write(*,*)
   write(*,*) 'Module matrix3'

   call system_clock(tin)
   call cpu_time(cin)
   !DIR$ FORCEINLINE
   do j=1,100
      do i=1,nvect
         M(1:3,1) = values(:,i)
         M(1:3,2) = values(:,i)
         M(1:3,3) = values(:,i)
         !DIR$ FORCEINLINE
         v_sub(i) = MOD_MATRIX33_det(M)
      end do
   end do
   call cpu_time(cout)
   call system_clock(tout)
   t_sub = tout-tin
   c_sub = cout-cin

   write(*,'(a,i10,a,i10,a,f10.3,a)')       't-sub =',t_sub,', t-mac = ',t_mac,', 100% +',100*dble(t_sub-t_mac)/dble(t_mac),'%'
   write(*,'(a,f10.7,a,f10.7,a,f10.3,a)')   'c-sub =',c_sub,', c-mac = ',c_mac,', 100% +',100*dble(c_sub-c_mac)/dble(c_mac),'%'

   write(*,*)
   write(*,*) 'Contained matrix3'

   call system_clock(tin)
   call cpu_time(cin)
   !DIR$ FORCEINLINE
   do j=1,100
      do i=1,nvect
         M(1:3,1) = values(:,i)
         M(1:3,2) = values(:,i)
         M(1:3,3) = values(:,i)
         !DIR$ FORCEINLINE
         v_sub(i) = CONT_MATRIX33_det(M)
      end do
   end do
   call cpu_time(cout)
   call system_clock(tout)
   t_sub = tout-tin
   c_sub = cout-cin

   write(*,'(a,i10,a,i10,a,f10.3,a)')       't-sub =',t_sub,', t-mac = ',t_mac,', 100% +',100*dble(t_sub-t_mac)/dble(t_mac),'%'
   write(*,'(a,f10.7,a,f10.7,a,f10.3,a)')   'c-sub =',c_sub,', c-mac = ',c_mac,', 100% +',100*dble(c_sub-c_mac)/dble(c_mac),'%'

   contains

   pure function CONT_MATRIX33_trace(M)
      real(8), intent(in) :: M(3, 3)
      real(8) :: CONT_MATRIX33_trace

      CONT_MATRIX33_trace = M(1,1)+M(2,2)+M(3,3)

   end function CONT_MATRIX33_trace

   pure subroutine CONT_MATRIX33_transpose(M, MT)
      real(8), intent(in)  :: M(3,3)
      real(8), intent(out) :: MT(3,3)

      MT(1,1) = M(1,1)
      MT(1,2) = M(2,1)
      MT(1,3) = M(3,1)
      MT(2,1) = M(1,2)
      MT(2,2) = M(2,2)
      MT(2,3) = M(3,2)
      MT(3,1) = M(1,3)
      MT(3,2) = M(2,3)
      MT(3,3) = M(3,3)
   end subroutine

   pure function CONT_MATRIX33_det(M)
      real(8), intent(in) :: M(3,3)
      real(8) :: CONT_MATRIX33_det

      CONT_MATRIX33_det = M(1,1)*(M(2,2)*M(3,3) - M(3,2)*M(2,3)) &
                        - M(1,2)*(M(2,1)*M(3,3) - M(3,1)*M(2,3)) &
                        + M(1,3)*(M(2,1)*M(3,2) - M(3,1)*M(2,2))

   end function CONT_MATRIX33_det

   pure subroutine CONT_MATRIX33_prod(ML, MR, MM)
      real(8), intent(in)  :: ML(3,3), MR(3,3)
      real(8), intent(out) :: MM(3,3)

      MM(1,1) = ML(1,1)*MR(1,1) + ML(1,2)*MR(2,1) + ML(1,3)*MR(3,1)
      MM(1,2) = ML(1,1)*MR(1,2) + ML(1,2)*MR(2,2) + ML(1,3)*MR(3,2)
      MM(1,3) = ML(1,1)*MR(1,3) + ML(1,2)*MR(2,3) + ML(1,3)*MR(3,3)
      MM(2,1) = ML(2,1)*MR(1,1) + ML(2,2)*MR(2,1) + ML(2,3)*MR(3,1)
      MM(2,2) = ML(2,1)*MR(1,2) + ML(2,2)*MR(2,2) + ML(2,3)*MR(3,2)
      MM(2,3) = ML(2,1)*MR(1,3) + ML(2,2)*MR(2,3) + ML(2,3)*MR(3,3)
      MM(3,1) = ML(3,1)*MR(1,1) + ML(3,2)*MR(2,1) + ML(3,3)*MR(3,1)
      MM(3,2) = ML(3,1)*MR(1,2) + ML(3,2)*MR(2,2) + ML(3,3)*MR(3,2)
      MM(3,3) = ML(3,1)*MR(1,3) + ML(3,2)*MR(2,3) + ML(3,3)*MR(3,3)

   end subroutine CONT_MATRIX33_prod

   pure function CONT_VECTOR3_norm(v)
      real(8), intent(in) :: v(3)
      real(8) :: CONT_VECTOR3_norm

      CONT_VECTOR3_norm = sqrt(v(1)*v(1) + v(2)*v(2) + v(3)*v(3))

   end function CONT_VECTOR3_norm

end program test_mac_vs_sub
