program string_slicing
   implicit none

   character(len=50) :: my_name

   my_name     = "<Heinrich Kraus>"

   call print_first_last_name(my_name)
   call print_first_last_name("..." // my_name // "...")


   contains

   subroutine print_string(str)
      character(len=*), intent(in) :: str

      write(*,*) len(str)

   end subroutine print_string

   subroutine print_first_last_name(full_name)
      character(len=*), intent(in) :: full_name

      integer :: i_space, i_start, i_end

      i_space = index(full_name, " ")
      i_start = index(full_name, "<")
      i_end   = index(full_name, ">")

      write(*,'(a, a)') "First name: ", full_name(i_start+1:i_space-1)
      write(*,'(a, a)') "Last name:  ", full_name(i_space+1:i_end-1)

      call print_string(full_name(i_start+1:i_space-1))
      call print_string(full_name(i_space+1:i_end-1))

   end subroutine print_first_last_name

end program string_slicing
