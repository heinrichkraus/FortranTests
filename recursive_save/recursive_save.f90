program recursive_save
    implicit none

    integer :: i
    logical :: larr(10)
    integer, allocatable, target, save :: arr(:,:), arr2(:,:)
    integer, pointer :: ptr1(:), ptr2(:)

    i = do_recursion(1)
    write(*,*) "i = ", i
    i = do_recursion(1)
    write(*,*) "i = ", i

    ! allocate(arr(3, 2))
    ! arr = 2
    ! write(*,*) "arr = ", arr

    ! ptr1 => arr(:, 1)
    ! ptr2 => arr(:, 2)

    ! allocate(arr2(6, 2))
    ! arr2 = 0
    ! nullify(ptr1, ptr2)
    ! arr2(1:3,:) = arr
    ! call move_alloc(arr2, arr)
    ! ptr1 => arr(:, 1)
    ! ptr2 => arr(:, 2)

    ! write(*,*) "arr = ", arr
    ! call set(ptr1, size(ptr1, dim = 1), 1)
    ! call set(ptr2, size(ptr2, dim = 1), 2)
    ! write(*,*) "arr = ", arr

    ! call setval(ptr1(1), 2)

    ! nullify(ptr1, ptr2)

    ! write(*,*) "arr = ", arr

contains

    subroutine set(vec, lenvec, k)
        integer, intent(inout) :: vec(*)
        integer, intent(in) :: k, lenvec
        integer :: i

        do i = 1,lenvec
            vec(i) = k
        end do
    end subroutine

    recursive function do_recursion(depth) result (res)
        integer, intent(in) :: depth
        integer :: res
        integer, save, allocatable, target :: arr(:,:)
        integer, pointer :: ptr(:)

        if (depth .eq. 1 .and. .not. allocated(arr)) then !allocate & initialize everything
            allocate(arr(3, 3))
            arr = 0
        else if (depth .gt. size(arr, dim = 2)) then !return depth at which we stopped
            res = depth
            return
        end if

        ptr => arr(:, depth)

        write(*, '(A, I3)') "IN:  depth = ", depth
        write(*, '(A, I3, I3, I3, I3, I3, I3, I3, I3, I3, I3, I3)') "arr = ", arr
        call set(ptr, size(ptr, dim=1), depth)
        res = do_recursion(depth + 1)
        write(*, '(A, I3)') "OUT: depth = ", depth
        write(*, '(A, I3, I3, I3, I3, I3, I3, I3, I3, I3, I3, I3)') "arr = ", arr

        nullify(ptr)

    end function do_recursion

    subroutine setval(x, k)
        integer, intent(out) :: x
        integer, intent(in)  :: k

        x = k
    end subroutine

end program recursive_save