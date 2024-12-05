module realloc_array
    implicit none

    private

    interface realloc_vector
       module procedure realloc_vector_int, realloc_vector_real
    end interface realloc_vector

    interface realloc_matrix
       module procedure &
          realloc_matrix_int, &
          realloc_matrix_real, &
          realloc_matrixptr_int, &
          realloc_matrixptr_real
    end interface realloc_matrix

    public :: realloc_vector, realloc_matrix, print_array

 contains

    subroutine realloc_vector_int(A, new_size)
       integer, allocatable, intent(inout) :: A(:)
       integer,    optional, intent(in)    :: new_size

       integer, allocatable :: Atmp(:)
       integer              :: old_size

       old_size = size(A)
       if (.not. present(new_size)) return
       if (new_size .lt. old_size) return
       allocate(Atmp(new_size))
       Atmp = 0
       Atmp(1:old_size) = A
       call move_alloc(Atmp, A)

    end subroutine realloc_vector_int

    subroutine realloc_vector_real(A, new_size)
       real,    allocatable, intent(inout) :: A(:)
       integer, optional,    intent(in)    :: new_size

       real, allocatable :: Atmp(:)
       integer           :: old_size

       old_size = size(A)
       if (.not. present(new_size)) return
       if (new_size .lt. old_size) return
       allocate(Atmp(new_size))
       Atmp = 0
       Atmp(1:old_size) = A
       call move_alloc(Atmp, A)

    end subroutine realloc_vector_real

    subroutine realloc_matrix_real(A, nrow, ncol)
       real,    allocatable, intent(inout) :: A(:,:)
       integer, optional,    intent(in)    :: nrow, ncol

       real,    allocatable :: Atmp(:,:)
       integer              :: new_nrow, new_ncol, old_nrow, old_ncol, i, j

       old_nrow = size(A, dim = 1)
       old_ncol = size(A, dim = 2)

       if (present(nrow)) then
          new_nrow = nrow
       else
          new_nrow = old_nrow
       end if

       if (present(ncol)) then
          new_ncol = ncol
       else
          new_ncol = old_ncol
       end if

       if (old_nrow .gt. new_nrow .or. old_ncol .gt. new_ncol) return

       allocate(Atmp(new_nrow, new_ncol))
       Atmp = 0
       do j = 1,old_ncol
          do i = 1,old_nrow
             Atmp(i, j) = A(i, j)
          end do
       end do
       call move_alloc(Atmp, A)

    end subroutine realloc_matrix_real

    subroutine realloc_matrix_int(A, nrow, ncol)
       integer, allocatable, intent(inout) :: A(:,:)
       integer, optional,    intent(in)    :: nrow, ncol

       integer, allocatable :: Atmp(:,:)
       integer              :: new_nrow, new_ncol, old_nrow, old_ncol, i, j

       old_nrow = size(A, dim = 1)
       old_ncol = size(A, dim = 2)

       if (present(nrow)) then
          new_nrow = nrow
       else
          new_nrow = old_nrow
       end if

       if (present(ncol)) then
          new_ncol = ncol
       else
          new_ncol = old_ncol
       end if

       if (old_nrow .gt. new_nrow .or. old_ncol .gt. new_ncol) return

       allocate(Atmp(new_nrow, new_ncol))
       Atmp = 0
       do j = 1,old_ncol
          do i = 1,old_nrow
             Atmp(i, j) = A(i, j)
          end do
       end do
       call move_alloc(Atmp, A)

    end subroutine realloc_matrix_int

    subroutine realloc_matrixptr_int(A, nrow, ncol)
       integer, pointer,  intent(inout) :: A(:,:)
       integer, optional, intent(in)    :: nrow, ncol

       integer, pointer :: Atmp(:,:)
       integer          :: new_nrow, new_ncol, old_nrow, old_ncol, i, j

       if (.not. associated(A)) then
          allocate(A(nrow, ncol))
          A = 0
          return
       end if

       old_nrow = size(A, dim = 1)
       old_ncol = size(A, dim = 2)

       if (present(nrow)) then
          new_nrow = nrow
       else
          new_nrow = old_nrow
       end if

       if (present(ncol)) then
          new_ncol = ncol
       else
          new_ncol = old_ncol
       end if

       if (old_nrow .gt. new_nrow .or. old_ncol .gt. new_ncol) return

       allocate(Atmp(new_nrow, new_ncol))
       Atmp = 0
       do j = 1,old_ncol
          do i = 1,old_nrow
             Atmp(i, j) = A(i, j)
          end do
       end do

       deallocate(A)

       A => Atmp

    end subroutine realloc_matrixptr_int

    subroutine realloc_matrixptr_real(A, nrow, ncol)
       real,    pointer,  intent(inout) :: A(:,:)
       integer, optional, intent(in)    :: nrow, ncol

       real,    pointer :: Atmp(:,:)
       integer          :: new_nrow, new_ncol, old_nrow, old_ncol, i, j

       if (.not. associated(A)) then
          allocate(A(nrow, ncol))
          A = 0
          return
       end if

       old_nrow = size(A, dim = 1)
       old_ncol = size(A, dim = 2)

       if (present(nrow)) then
          new_nrow = nrow
       else
          new_nrow = old_nrow
       end if

       if (present(ncol)) then
          new_ncol = ncol
       else
          new_ncol = old_ncol
       end if

       if (old_nrow .gt. new_nrow .or. old_ncol .gt. new_ncol) return

       allocate(Atmp(new_nrow, new_ncol))
       Atmp = 0
       do j = 1,old_ncol
          do i = 1,old_nrow
             Atmp(i, j) = A(i, j)
          end do
       end do

       deallocate(A)

       A => Atmp

    end subroutine realloc_matrixptr_real

    subroutine print_array(arr)
        integer, intent(in) :: arr(:,:)
        integer :: nrow, i

        nrow = size(arr, dim = 1)

        do i = 1,nrow
            write(*,*) "i = ", i, ":", arr(i, :)
        end do
    end subroutine

 end module realloc_array

program ReadVector
    use realloc_array

    implicit none

    integer, allocatable :: arr(:, :)
    integer :: i, a, b, count
    real :: t0, t1


    allocate(arr(1, 2))
    count = 0
    do while (.true.)
        print*, "Please enter a number: "
        read(*,'(I4, I4)') a, b

        write(*, '(A, I4, I4)') "a, b = ", a, b

        if (a .eq. 0 .and. b .eq. 0) exit

        count = count + 1

        arr(count, 1) = a
        arr(count, 2) = b

        if (size(arr, dim = 1) .eq. count) then
            call realloc_matrix(arr, nrow = 2 * count)
        end if

        call print_array(arr)

    end do

    ! write(*, *) "vec = ", vec
    ! do i = 1,size(arr, dim=1)
    !     write(*, *) "arr(", i, ", :) = ", arr(i, :)
    ! end do
    ! write(*, *) "arr = ", arr


end program ReadVector

