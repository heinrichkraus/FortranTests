module PointTypes
   type :: Point3D
      integer :: id
      real    :: x
      real    :: y
      real    :: z
   end type
end module

program main
   use PointTypes

   implicit none

   type(Point3D) :: points(10)
   real, allocatable :: xs(:)
   integer :: i

   do i = 1,10
      call random_number(points(i)%x)
      call random_number(points(i)%y)
      call random_number(points(i)%z)
      points(i)%id = i
   end do

   write(*,*) points%x
   write(*,*) points%id

   xs = points%x

   print*, "xs = ", xs


contains


end program main
