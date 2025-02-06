module math33

   public

   contains

   function mat33_vecprod_fun(A, x) result(y)
      real(8), intent(in) :: A(3,3), x(3)
      real(8) :: y(3)

      y(1) = A(1, 1) * x(1) + A(1, 2) * x(2) + A(1, 3) * x(3)
      y(2) = A(2, 1) * x(1) + A(2, 2) * x(2) + A(2, 3) * x(3)
      y(3) = A(3, 1) * x(1) + A(3, 2) * x(2) + A(3, 3) * x(3)

   end function mat33_vecprod_fun

   subroutine mat33_vecprod_sub(A, x, y)
      real(8), intent(in)  :: A(3,3), x(3)
      real(8), intent(out) :: y(3)

      y(1) = A(1, 1) * x(1) + A(1, 2) * x(2) + A(1, 3) * x(3)
      y(2) = A(2, 1) * x(1) + A(2, 2) * x(2) + A(2, 3) * x(3)
      y(3) = A(3, 1) * x(1) + A(3, 2) * x(2) + A(3, 3) * x(3)

   end subroutine mat33_vecprod_sub

   function mat33_transpose_fun(A) result(B)
      real(8), intent(in) :: A(3,3)
      real(8) :: B(3,3)

      B(1,1) = A(1,1)
      B(1,2) = A(2,1)
      B(1,3) = A(3,1)
      B(2,1) = A(1,2)
      B(2,2) = A(2,2)
      B(2,3) = A(3,2)
      B(3,1) = A(1,3)
      B(3,2) = A(2,3)
      B(3,3) = A(3,3)

   end function mat33_transpose_fun

   subroutine mat33_transpose_sub(A, B)
      real(8), intent(in)  :: A(3,3)
      real(8), intent(out) :: B(3,3)

      B(1,1) = A(1,1)
      B(1,2) = A(2,1)
      B(1,3) = A(3,1)
      B(2,1) = A(1,2)
      B(2,2) = A(2,2)
      B(2,3) = A(3,2)
      B(3,1) = A(1,3)
      B(3,2) = A(2,3)
      B(3,3) = A(3,3)

   end subroutine mat33_transpose_sub

   subroutine vec3_cross_product(v1, v2, v3)
      real(8), intent(in)  :: v1(3), v2(3)
      real(8), intent(out) :: v3(3)

      v3(1) = v1(2) * v2(3) - v1(3) * v2(2)
      v3(2) = v1(3) * v2(1) - v1(1) * v2(3)
      v3(3) = v1(1) * v2(2) - v1(2) * v2(1)

   end subroutine vec3_cross_product

end module math33
