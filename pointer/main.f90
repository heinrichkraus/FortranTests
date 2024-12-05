program pointers
   implicit none

   real, pointer :: Y(:,:),  Yi(:)
   integer :: i, j, N, M

   N = 3
   M = 10

   allocate(Y(N, M))

   do i = 1,M
      do j = 1,N
         call random_number(Y(j, i))
      end do
   end do


   write(*,*) Y
   do i = 1,M
      Yi => Y(:, i)
      call ascending(Yi, i)
      call noname(Yi, Yi(1))
      nullify(Yi)
   end do
   write(*,*) Y

   contains

   subroutine ascending(Y, i)
      real, intent(inout) :: Y(:)
      integer, intent(in) :: i

      integer :: j

      do j = 1,size(Y)
         Y(j) = real(i+j-1)
      end do

   end subroutine ascending

   subroutine noname(Yi, a)
      real, intent(in)  :: Yi(:)
      real, intent(out) :: a

      a = Yi(1) + Yi(2)

   end subroutine noname

end program pointers

