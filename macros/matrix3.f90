module matrix3
   implicit none

contains

   pure function MOD_MATRIX33_trace(M)
      real(8), intent(in) :: M(3, 3)
      real(8) :: MOD_MATRIX33_trace

      MOD_MATRIX33_trace = M(1,1)+M(2,2)+M(3,3)

   end function MOD_MATRIX33_trace

   pure subroutine MOD_MATRIX33_transpose(M, MT)
      real(8), intent(in)  :: M(3,3)
      real(8), intent(out) :: MT(3,3)

      MT(1,1) = M(1,1)
      MT(1,2) = M(2,1)
      MT(1,3) = M(3,1)
      MT(2,1) = M(1,2)
      MT(2,2) = M(2,2)
      MT(2,3) = M(3,2)
      MT(3,1) = M(1,3)
      MT(3,2) = M(2,3)
      MT(3,3) = M(3,3)
   end subroutine

   pure function MOD_MATRIX33_det(M)
      real(8), intent(in) :: M(3,3)
      real(8) :: MOD_MATRIX33_det

      MOD_MATRIX33_det = M(1,1)*(M(2,2)*M(3,3) - M(3,2)*M(2,3)) &
                       - M(1,2)*(M(2,1)*M(3,3) - M(3,1)*M(2,3)) &
                       + M(1,3)*(M(2,1)*M(3,2) - M(3,1)*M(2,2))

   end function MOD_MATRIX33_det

   pure subroutine MOD_MATRIX33_prod(ML, MR, MM)
      real(8), intent(in)  :: ML(3,3), MR(3,3)
      real(8), intent(out) :: MM(3,3)

      MM(1,1) = ML(1,1)*MR(1,1) + ML(1,2)*MR(2,1) + ML(1,3)*MR(3,1)
      MM(1,2) = ML(1,1)*MR(1,2) + ML(1,2)*MR(2,2) + ML(1,3)*MR(3,2)
      MM(1,3) = ML(1,1)*MR(1,3) + ML(1,2)*MR(2,3) + ML(1,3)*MR(3,3)
      MM(2,1) = ML(2,1)*MR(1,1) + ML(2,2)*MR(2,1) + ML(2,3)*MR(3,1)
      MM(2,2) = ML(2,1)*MR(1,2) + ML(2,2)*MR(2,2) + ML(2,3)*MR(3,2)
      MM(2,3) = ML(2,1)*MR(1,3) + ML(2,2)*MR(2,3) + ML(2,3)*MR(3,3)
      MM(3,1) = ML(3,1)*MR(1,1) + ML(3,2)*MR(2,1) + ML(3,3)*MR(3,1)
      MM(3,2) = ML(3,1)*MR(1,2) + ML(3,2)*MR(2,2) + ML(3,3)*MR(3,2)
      MM(3,3) = ML(3,1)*MR(1,3) + ML(3,2)*MR(2,3) + ML(3,3)*MR(3,3)

   end subroutine MOD_MATRIX33_prod

   pure function MOD_VECTOR3_norm(v)
      real(8), intent(in) :: v(3)
      real(8) :: MOD_VECTOR3_norm

      MOD_VECTOR3_norm = sqrt(v(1)*v(1) + v(2)*v(2) + v(3)*v(3))

   end function MOD_VECTOR3_norm

end module matrix3
