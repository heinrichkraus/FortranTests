! This program demonstrates one pitfall in Fortran development.
! In the following, an array of given size is allocated.
program array_bounds

   integer, allocatable, dimension(:) :: array
   integer, parameter :: N = 100
   integer :: M

   allocate(array(0:N))

   M = N+30000
   call test4(array(10:20))
   call test4(array)
   call test1(array(1), M)
   call test2(array(10), M)
   call test3(array, M)

   deallocate(array)

contains

   subroutine test4(x)
      integer, intent(inout) :: x(:)

      ! write(*,*) size(x)
      ! write(*,*) loc(x)
      write(*,*) lbound(x)
      write(*,*) ubound(x)

   end subroutine test4

   ! In this subroutine, the input array is assumed to be of size l. There is no size check for the
   ! input such that l can be bigger or smaller than the actual array.
   ! Because of that, size(x) = l always holds, and the size check does not make sense.
   subroutine test1(x, l)
      integer, intent(inout) :: x(1:l), l
      integer :: ii

      write(*,*) size(x,1)

      do ii = 1,l
         if(ii>size(x,1)) then
            write(*,*) "ERROR"
            return
         end if
         x(ii) = ii
      end do

      write(*,*) "NO ERROR"

   end subroutine test1

   ! Identital to test1
   subroutine test2(x, l)
      integer, dimension(1:l) :: x
      integer :: l
      integer :: ii

      write(*,*) size(x,1)

      do ii = 1,l
         if(ii>size(x,1)) then
            write(*,*) "ERROR"
            return
         end if
         x(ii) = ii
      end do

      write(*,*) "NO ERROR"

   end subroutine test2

   ! Here, we do not assume the size of the array inside the subroutine. Instead, the size is
   ! transfered from the caller-routine such that the check size(x) <= l makes sense. This should
   ! be the prefered way of implementing such subroutines.
   ! In fact, passing the length l in this case is not necessary and should be avoided if it is not
   ! strictly necessary.
   subroutine test3(x, l)
      integer :: x(:), l
      integer :: ii

      write(*,*) size(x,1)

      do ii = 1,l
         if(ii>size(x,1)) then
            write(*,*) "ERROR"
            return
         end if
         x(ii) = ii
      end do

      write(*,*) "NO ERROR"

   end subroutine test3

end program array_bounds
