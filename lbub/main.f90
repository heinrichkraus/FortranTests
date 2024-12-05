program lbub
   implicit none

   integer, allocatable, dimension(:) :: array_alloc
   integer, pointer,     dimension(:) :: array_pointer

   allocate(array_alloc(-5:12))
   print*, "bounds = ", lbound(array_alloc), ubound(array_alloc)
   call show_bounds_alloc(array_alloc)

   allocate(array_pointer(-5:12))
   print*, "bounds = ", lbound(array_pointer), ubound(array_pointer)
   call show_bounds_pointer(array_pointer)

   contains

   subroutine show_bounds_alloc(arr)
      integer, intent(in), allocatable :: arr(:)

      print*, "bounds = ", lbound(arr), ubound(arr)

   end subroutine show_bounds_alloc

   subroutine show_bounds_pointer(arr)
      integer, intent(in), pointer :: arr(:)

      print*, "bounds = ", lbound(arr), ubound(arr)

   end subroutine show_bounds_pointer

end program lbub
