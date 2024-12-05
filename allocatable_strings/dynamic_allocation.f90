module strdump
   implicit none

   character(len=:), allocatable :: STR_vec
   integer,          allocatable :: STR_pointer(:), STR_length(:)
   integer                       :: STR_nb_defined = 0

   private STR_vec, STR_pointer, STR_length, STR_nb_defined, STR_init
   public  STR_write, STR_read

contains

   subroutine STR_init()
      STR_vec = ""
      allocate(STR_pointer(0), STR_length(0))
      STR_nb_defined = 0
   end subroutine STR_init

   subroutine STR_write(str, ptr)
      character(len=*), intent(in)  :: str
      integer,          intent(out) :: ptr

      if (STR_nb_defined .eq. 0) call STR_init()

      STR_nb_defined = STR_nb_defined + 1
      ptr = STR_nb_defined

      STR_pointer = [STR_pointer, len(STR_vec)+1]
      STR_length  = [STR_length,  len_trim(str)]
      STR_vec     = STR_vec // str

   end subroutine STR_write

   subroutine STR_read(i, str)
      integer,          intent(in)  :: i
      character(len=*), intent(out) :: str

      str = STR_vec(STR_pointer(i):str_pointer(i)+str_length(i)-1)

   end subroutine STR_read

end module strdump

program dynamic_allocation
   use strdump

   implicit none

   character(128) :: strvec(10), str
   integer :: i, ptr

   strvec(1) = "abc123"
   strvec(2) = "hello"
   strvec(3) = "myworld"
   strvec(4) = "somethingverylong with some spcaes goes here"
   strvec(5) = "short"
   strvec(6) = "my name is something else"
   strvec(7) = "HEINRICH"
   strvec(8) = "KRAUS"
   strvec(9) = "hey"
   strvec(10) = "I SHOUT LIKE FORTRAN"

   do i = 1,10
      call STR_write(strvec(i), ptr)
   end do

   do i = 1,10
      call STR_read(i, str)
      write(*,*) str
   end do

end program dynamic_allocation
