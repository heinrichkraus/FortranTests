program array_constructor
   implicit none

   real, allocatable :: Ts(:)
   real :: Tmin, Tmax
   integer :: i, N

   N    = 17
   Tmin = 1.0
   Tmax = 5.0

   Ts = [(Tmin + (i-1) * (Tmax-Tmin)/(N-1), i=1,N)]

   write(*,*) size(Ts), Ts

   Ts = [Tmin]
   write(*,*) size(Ts), Ts

end program array_constructor


