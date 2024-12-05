module kslinalg
    implicit none

    public ksdot

contains

function ksdot(x, y, n)
    real(8) :: ksdot
    integer, intent(in) :: n
    real(8), intent(in) :: x(n), y(n)
    real(8) :: z(n)
    integer :: i

    ! do i = 1,n
    !     z(i) = x(i) * y(i)
    ! end do
    z = x * y
    ksdot = sum(z)

    return

end function ksdot

end module kslinalg