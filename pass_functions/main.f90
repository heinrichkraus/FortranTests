program pass_functions
   implicit none

   write(*,*) evaluate_function(f1, 3.0)
   write(*,*) evaluate_function(f2, 3.0)

   contains

   real function f1(x)
      real, intent(in) :: x
      f1 = x*x - 1
   end function

   real function f2(x)
      real, intent(in) :: x
      f2 = x*x + 1
   end function

   real function evaluate_function(f, x)
      real, intent(in) :: x
      interface
         real function f(x)
            real, intent(in) :: x
         end function
      end interface

      evaluate_function = f(x)
   end function

end program pass_functions
