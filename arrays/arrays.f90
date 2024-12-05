program arrays
   implicit none

   real :: xvec(5), yvec(5), zvec(5)
   real :: xmat(5, 1), ymat(5, 1), zmat(5, 1)

   integer :: i

   do i = 1,5
      call random_number(xvec(i))
      call random_number(yvec(i))
      call random_number(zvec(i))
      xmat(i, 1) = xvec(i)
      ymat(i, 1) = yvec(i)
      zmat(i, 1) = zvec(i)
   end do

   call array_sub(xvec, yvec, zvec, 5)
   write(*,*) repeat("-", 120)
   call array_sub([xvec(4)], [yvec(4)], [zvec(4)], 1)
   write(*,*) repeat("-", 120)
   call array_sub(xmat(:, 1), ymat(:, 1), zmat(:, 1), 5)

   contains

   subroutine array_sub(x, y, z, n)
      real,    intent(in) :: x(:), y(:), z(:)
      integer, intent(in) :: n
      integer :: i
      do i = 1,n
         write(*, *) x(i), y(i), z(i)
      end do
   end subroutine array_sub

end program arrays
