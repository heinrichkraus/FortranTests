program slicing
   implicit none

   integer :: N, M, L, N_samples, len_str
   character(10) :: str
   real :: t0, t1
   real, allocatable :: arr(:, :), LARGE_ARRAY(:), LARGE_ARRAY_REF(:)
   integer i, j, k

   call get_command_argument(1, value=str, length=len_str)
   read(str(1:len_str),*) N
   call get_command_argument(2, value=str, length=len_str)
   read(str(1:len_str),*) M
   call get_command_argument(3, value=str, length=len_str)
   read(str(1:len_str),*) L

   ! N = 1500      ! nb of rows in array
   ! M = 1000000   ! nb of cols in array
   ! L = 10000     ! actual nb of cols used
   N_samples = 50

   allocate(arr(N, M), LARGE_ARRAY(N*L), LARGE_ARRAY_REF(N*L))

   k = 0
   do j=1,L
      do i=1,N
         k = k+1
         arr(i, j) = sqrt(real(i * j)) / (real(i+j))
         LARGE_ARRAY_REF(k) = arr(i, j)
      end do
   end do

   call cpu_time(t0)
   do i = 1,N_samples
      call write_to_large(arr(1:N, 1:L))
   end do
   call cpu_time(t1)
   call test_arrays()
   write(*,*) "Elapsed time :           ", t1-t0
   write(*,*) "Avg. elapsed time :      ", (t1-t0)/N_samples

   call cpu_time(t0)
   do i = 1,N_samples
      call write_to_large_ptr(arr(1, 1), N, L)
   end do
   call cpu_time(t1)
   call test_arrays()
   write(*,*) "Elapsed time (ptr):      ", t1-t0
   write(*,*) "Avg. elapsed time (ptr): ", (t1-t0)/N_samples

contains

   subroutine test_arrays()
      integer :: i
      do i = 1,size(LARGE_ARRAY)
         if (LARGE_ARRAY(i) .ne. LARGE_ARRAY_REF(i)) then
            print*, "FAIL"
            return
         end if
      end do
      print*, "SUCCESS"
   end subroutine test_arrays

   subroutine write_to_large_ptr(a, nrow, ncol)
      real,    intent(in) :: a(nrow, ncol)
      integer, intent(in) :: nrow, ncol

      integer :: i, j, cnt

      cnt = 0
      do j = 1,ncol
         do i = 1,nrow
            cnt = cnt + 1
            LARGE_ARRAY(cnt) = a(i, j)
         end do
      end do

   end subroutine write_to_large_ptr

   subroutine write_to_large(a)
      real,    intent(in) :: a(:, :)

      integer :: nrow, ncol, i, j, cnt

      nrow = size(a, 1)
      ncol = size(a, 2)

      cnt = 0
      do j = 1,ncol
         do i = 1,nrow
            cnt = cnt + 1
            LARGE_ARRAY(cnt) = a(i, j)
         end do
      end do

   end subroutine write_to_large

end program slicing
