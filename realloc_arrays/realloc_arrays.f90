module realloc_array
    implicit none

    private

    interface reallocate
       module procedure &
          reallocate_array1_int, &
          reallocate_array1_real, &
          reallocate_array2_int, &
          reallocate_array2_real, &
          reallocate_array2ptr_int, &
          reallocate_array2ptr_real, &
          reallocate_array3_real
    end interface reallocate

    public :: reallocate

 contains

    subroutine reallocate_array1_int(A, new_size)
       integer, allocatable, intent(inout) :: A(:)
       integer,    optional, intent(in)    :: new_size

       integer, allocatable :: Atmp(:)
       integer           :: i, lb, ub

       if (.not. present(new_size)) return

       lb = lbound(A, dim=1)
       ub = ubound(A, dim=1)

       allocate(Atmp(lb:new_size))
       Atmp = 0
       do i = lb,min(ub, new_size)
          Atmp(i) = A(i)
       end do
       call move_alloc(Atmp, A)

    end subroutine reallocate_array1_int

    subroutine reallocate_array1_real(A, new_size)
       real,    allocatable, intent(inout) :: A(:)
       integer, optional,    intent(in)    :: new_size

       real, allocatable :: Atmp(:)
       integer           :: i, lb, ub

       if (.not. present(new_size)) return

       lb = lbound(A, dim=1)
       ub = ubound(A, dim=1)

       allocate(Atmp(lb:new_size))
       Atmp = 0
       do i = lb,min(ub, new_size)
          Atmp(i) = A(i)
       end do
       call move_alloc(Atmp, A)

    end subroutine reallocate_array1_real

    subroutine reallocate_array2_real(A, nrow, ncol)
       real,    allocatable, intent(inout) :: A(:,:)
       integer, optional,    intent(in)    :: nrow, ncol

       real,    allocatable :: Atmp(:,:)
       integer              :: lb(2), ub(2), ub_old(2), i, j

       lb = lbound(A)
       ub = ubound(A)
       ub_old = ub

       if (present(nrow)) then
          ub(1) = nrow
       end if

       if (present(ncol)) then
          ub(2) = ncol
       end if

       allocate(Atmp(lb(1):ub(1), lb(2):ub(2)))
       Atmp = 0
       do j = lb(2),min(ub(2), ub_old(2))
          do i = lb(1),min(ub(1), ub_old(1))
             Atmp(i, j) = A(i, j)
          end do
       end do
       call move_alloc(Atmp, A)

    end subroutine reallocate_array2_real

    subroutine reallocate_array2_int(A, nrow, ncol)
       integer, allocatable, intent(inout) :: A(:,:)
       integer, optional,    intent(in)    :: nrow, ncol

       integer, allocatable :: Atmp(:,:)
       integer              :: lb(2), ub(2), ub_old(2), i, j

       lb = lbound(A)
       ub = ubound(A)
       ub_old = ub

       if (present(nrow)) then
          ub(1) = nrow
       end if

       if (present(ncol)) then
          ub(2) = ncol
       end if

       allocate(Atmp(lb(1):ub(1), lb(2):ub(2)))
       Atmp = 0
       do j = lb(2),min(ub(2), ub_old(2))
          do i = lb(1),min(ub(1), ub_old(1))
             Atmp(i, j) = A(i, j)
          end do
       end do
       call move_alloc(Atmp, A)

    end subroutine reallocate_array2_int

    subroutine reallocate_array2ptr_int(A, nrow, ncol)
       integer, pointer,  intent(inout) :: A(:,:)
       integer, optional, intent(in)    :: nrow, ncol

       integer, pointer :: Atmp(:,:)
       integer          :: lb(2), ub(2), ub_old(2), i, j

       lb = lbound(A)
       ub = ubound(A)
       ub_old = ub

       if (present(nrow)) then
          ub(1) = nrow
       end if

       if (present(ncol)) then
          ub(2) = ncol
       end if

       allocate(Atmp(lb(1):ub(1), lb(2):ub(2)))
       Atmp = 0
       do j = lb(2),min(ub(2), ub_old(2))
          do i = lb(1),min(ub(1), ub_old(1))
             Atmp(i, j) = A(i, j)
          end do
       end do

       deallocate(A)

       A => Atmp

    end subroutine reallocate_array2ptr_int

    subroutine reallocate_array2ptr_real(A, nrow, ncol)
       real,    pointer,  intent(inout) :: A(:,:)
       integer, optional, intent(in)    :: nrow, ncol

       real,    pointer :: Atmp(:,:)
       integer          :: lb(2), ub(2), ub_old(2), i, j

       lb = lbound(A)
       ub = ubound(A)
       ub_old = ub

       if (present(nrow)) then
          ub(1) = nrow
       end if

       if (present(ncol)) then
          ub(2) = ncol
       end if

       allocate(Atmp(lb(1):ub(1), lb(2):ub(2)))
       Atmp = 0
       do j = lb(2),min(ub(2), ub_old(2))
          do i = lb(1),min(ub(1), ub_old(1))
             Atmp(i, j) = A(i, j)
          end do
       end do

       deallocate(A)

       A => Atmp

    end subroutine reallocate_array2ptr_real

    subroutine reallocate_array3_real(A, nrow, ncol, ndepth)
       real,    allocatable, intent(inout) :: A(:,:,:)
       integer, optional,    intent(in)    :: nrow, ncol, ndepth

       real,    allocatable :: Atmp(:,:,:)
       integer              :: lb(3), ub(3), ub_old(3), i, j, k

       lb = lbound(A)
       ub = ubound(A)
       ub_old = ub

       if (present(nrow)) then
          ub(1) = nrow
       end if

       if (present(ncol)) then
          ub(2) = ncol
       end if

       if (present(ndepth)) then
          ub(3) = ndepth
       end if

       allocate(Atmp(lb(1):ub(1), lb(2):ub(2), lb(3):ub(3)))
       Atmp = 0
       do k = lb(3),min(ub(3), ub_old(3))
          do j = lb(2),min(ub(2), ub_old(2))
             do i = lb(1),min(ub(1), ub_old(1))
                Atmp(i, j, k) = A(i, j, k)
             end do
          end do
       end do
       call move_alloc(Atmp, A)

    end subroutine reallocate_array3_real

    subroutine reallocate_array3_int(A, nrow, ncol, ndepth)
       integer, allocatable, intent(inout) :: A(:,:,:)
       integer, optional,    intent(in)    :: nrow, ncol, ndepth

       integer, allocatable :: Atmp(:,:,:)
       integer              :: lb(3), ub(3), ub_old(3), i, j, k

       lb = lbound(A)
       ub = ubound(A)
       ub_old = ub

       if (present(nrow)) then
          ub(1) = nrow
       end if

       if (present(ncol)) then
          ub(2) = ncol
       end if

       if (present(ndepth)) then
          ub(3) = ndepth
       end if

       allocate(Atmp(lb(1):ub(1), lb(2):ub(2), lb(3):ub(3)))
       Atmp = 0
       do k = lb(3),min(ub(3), ub_old(3))
          do j = lb(2),min(ub(2), ub_old(2))
             do i = lb(1),min(ub(1), ub_old(1))
                Atmp(i, j, k) = A(i, j, k)
             end do
          end do
       end do
       call move_alloc(Atmp, A)

    end subroutine reallocate_array3_int

 end module realloc_array

program main
    use realloc_array
    implicit none

    integer, allocatable :: arr(:, :), vec(:)
    integer :: i, j

    allocate(arr(1:2, 0:5))
    allocate(vec(0:5))

    do j = 0,5
        do i = 1,2
            arr(i, j) = i + j
        end do
        vec(j) = j
    end do

    write(*, *) "arr = ", arr
    write(*, *) "lb = ", lbound(arr)
    write(*, *) "ub = ", ubound(arr)
    write(*, *) "sz = ", size(arr, dim = 1), size(arr, dim = 2)

    call reallocate(arr, ncol = 3) !Spalten löschen

    write(*, *) "arr = ", arr
    write(*, *) "lb = ", lbound(arr)
    write(*, *) "ub = ", ubound(arr)
    write(*, *) "sz = ", size(arr, dim = 1), size(arr, dim = 2)

    call reallocate(arr, nrow = 1) !Zeilen vergrößern

    write(*, *) "arr = ", arr
    write(*, *) "lb = ", lbound(arr)
    write(*, *) "ub = ", ubound(arr)
    write(*, *) "sz = ", size(arr, dim = 1), size(arr, dim = 2)

    ! Für jede Dimension gilt: sz = ub - lb + 1

    write(*,*) "vec = ", vec
    call reallocate(vec, new_size=10)
    write(*,*) "vec = ", vec
    call reallocate(vec, new_size=3)
    write(*,*) "vec = ", vec


end program main
