module points
   implicit none

   type::Point
      real::x, y, z
      integer::id
   end type Point

   integer :: point_counter = 0

   ! private point_counter

interface
   module function new_point(x, y, z) result(p)
      real, intent(in) :: x, y, z
      type(Point) :: p
   end function

   module function point_dist(p1, p2) result(dist)
      type(Point), intent(in) :: p1, p2
      real :: dist
   end function

   module function combine_points(p1, p2) result(p_sum)
      type(Point), intent(in) :: p1, p2
      type(Point) :: p_sum
   end function

   module function point2container(p) result(c)
      use container
      type(Point), intent(in) :: p
      type(ContainerType)     :: c
   end function
end interface

end module points
