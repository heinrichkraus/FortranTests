module class_Circle
   implicit none
   private
   real :: pi = 3.1415926535897931 ! Class-wide private constant

   public printstrvec

   type, public :: Circle
      real :: radius
      contains
      procedure area, print
   end type Circle
 contains
   real function area(this)
     class(Circle), intent(in) :: this
     area = pi * this%radius**2
   end function area

   subroutine print(this)
     class(Circle), intent(in) :: this
     print *, 'Circle: r = ', this%radius, ' area = ', this%area()
   end subroutine print

   subroutine printstrvec(strvec, n)
      character(len=*), intent(in) :: strvec(:)
      integer, intent(in) :: n

      integer :: i

      do i = 1,n
         write(*,*) trim(strvec(i))
      end do

   end subroutine printstrvec
 end module class_Circle


 program circle_test
   use class_Circle
   implicit none

   type(Circle) :: c     ! Declare a variable of type Circle.
   character(len=12800) :: strvec(5)

   c = Circle(1.5)       ! Use the implicit constructor, radius = 1.5.
   call c%print()        ! Call the type-bound subroutine

   strvec(1) = "abc123"
   strvec(2) = "hello"
   strvec(3) = "myworld"
   strvec(4) = "somethingverylong with some spcaes goes here"
   strvec(5) = "short"

   call printstrvec(strvec, 5)
 end program circle_test
