program submodules
   use points
   use container

   implicit none

   type(Point) :: p1, p2, p3
   type(ContainerType) :: c
   real :: dist

   p1 = new_point(1.0, 2.0, 3.0)
   p2 = new_point(2.0, 2.0, 1.0)

   dist = point_dist(p1, p2)

   p3 = combine_points(p1, p2)
   write(*,*) "p = ", p3%x, p3%y, p3%z

   c = point2container(p3)
   write(*,*) "c = ", c%id, c%value

end program submodules
