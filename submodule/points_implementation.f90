submodule (points) points_implementation

contains

   module function new_point(x, y, z) result(p)
      real, intent(in) :: x, y, z
      type(Point) :: p

      point_counter = point_counter + 1

      p%x = x
      p%y = y
      p%z = z
      p%id = point_counter
   end function

   module function point_dist(p1, p2) result(dist)
      type(Point), intent(in) :: p1, p2
      real :: dist

      dist = (p1%x-p2%x)**2 + (p1%y-p2%y)**2 + (p1%z-p2%z)**2
      dist = sqrt(dist)
   end function

   module function combine_points(p1, p2) result(p_sum)
      use container
      type(Point), intent(in) :: p1, p2
      type(Point) :: p_sum

      type(ContainerType) :: ct

      point_counter = point_counter + 1

      p_sum%x = p1%x + p2%x
      p_sum%y = p1%y + p2%y
      p_sum%z = p1%z + p2%z
      p_sum%id = point_counter

      ct%id = point_counter
      ct%value = point_counter
   end function

   module function point2container(p) result(c)
      use container
      type(Point), intent(in) :: p
      type(ContainerType)     :: c

      c%id = p%id
      c%value = p%id**2
   end function

end submodule
