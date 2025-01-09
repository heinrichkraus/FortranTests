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

   type(Point3D), allocatable :: points(:), subpoints(:)
   real, allocatable :: xs(:)
   integer :: i

   allocate(points(10))

   do i = 1,10
      call random_number(points(i)%x)
      call random_number(points(i)%y)
      call random_number(points(i)%z)
      points(i)%id = i
   end do

   xs = points%x
   print*, "xs = ", xs

   subpoints = points(1:5)
   xs = subpoints%x
   print*, "xs = ", xs

   xs = points(1:5)%x
   print*, "xs = ", xs

contains


end program main
