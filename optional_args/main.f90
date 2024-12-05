program optional_arguments
   implicit none

   real :: f, df

   call smooth_stepfun(1.0, 0.1, f=f)
   write(*,'(A, F6.2, A, F6.2)') "f = ", f, ", df = ", df

   call smooth_stepfun(0.01, 0.1, f=f, df=df)
   write(*,'(A, F6.2, A, F6.2)') "f = ", f, ", df = ", df

   call smooth_stepfun(0.001, 0.01, f=f, df=df)
   write(*,'(A, F6.2, A, F6.2)') "f = ", f, ", df = ", df

   contains

   subroutine smooth_stepfun(x, eps, f, df)
      real, intent(in) :: x, eps
      real, intent(out), optional :: f, df

      if (eps .le. 0.0) stop

      if (x .le. -eps) then
         if(present(f))  f  = 0.0
         if(present(df)) df = 0.0
         return
      end if

      if (x .ge. eps) then
         if(present(f))  f  = 1.0
         if(present(df)) df = 0.0
         return
      end if

      if(present(f))  f  = 0.5 * (1 + x/eps)
      if(present(df)) df = 0.5 / eps
   end subroutine
end program optional_arguments
