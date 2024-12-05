program array_bounds_FC

   integer, pointer, dimension(:) :: array

   call test(array)


   deallocate(array)

   write (*, *) "Hello world"

contains

   subroutine test(x)
      integer, pointer :: x(:)
      integer :: ii

      allocate (x(100))

      do ii = 1,103
         x(ii) = ii
      end do

   end subroutine test

end program array_bounds_FC
