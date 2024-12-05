module matrix
   implicit none

contains

   pure function MOD_MATRIX_trace(M)
      real(8), intent(in) :: M(:,:)
      real(8) :: MOD_MATRIX_trace

      integer :: i

      MOD_MATRIX_trace = 0.0

      if (size(M, dim=1) /= size(M, dim=2)) return

      do i=1,size(M,1)
         MOD_MATRIX_trace = MOD_MATRIX_trace + M(i,i)
      end do

   end function MOD_MATRIX_trace

   pure subroutine MOD_MATRIX_transpose(M, MT)
      real(8), intent(in)  :: M(:,:)
      real(8), intent(out) :: MT(:,:)

      integer :: i, j

      if (size(M, dim=1) /= size(MT, dim=2)) return
      if (size(M, dim=2) /= size(MT, dim=1)) return

      do j=1,size(M, dim=1)
         do i=1,size(M, dim=2)
            MT(i,j) = M(j,i)
         end do
      end do
   end subroutine

   subroutine MOD_MATRIX_prod(ML, MR, MM)
      real(8), intent(in)  :: ML(:,:), MR(:,:)
      real(8), intent(out) :: MM(:,:)

      integer :: MLrow, MLcol, MRrow, MRcol, i, j, k

      MLrow = size(ML, dim=1)
      MLcol = size(ML, dim=2)
      MRrow = size(MR, dim=1)
      MRcol = size(MR, dim=2)

      if (MLcol /= MRrow) return
      if (MLrow /= size(MM, dim=1)) return
      if (MRcol /= size(MM, dim=2)) return

      do j=1,MRcol
         do i=1,MLrow
            MM(i,j) = 0.0
            do k=1,MLcol
               MM(i,j) = MM(i,j) + ML(i,k)*MR(k,j)
            end do
         end do
      end do

   end subroutine MOD_MATRIX_prod

   pure function MOD_VECTOR_norm(v)
      real(8), intent(in) :: v(:)
      real(8) :: MOD_VECTOR_norm
      integer :: i

      MOD_VECTOR_norm = 0.0

      do i=1,size(v,1)
         MOD_VECTOR_norm = MOD_VECTOR_norm + v(i) * v(i)
      end do

      MOD_VECTOR_norm = sqrt(MOD_VECTOR_norm)

   end function MOD_VECTOR_norm

end module matrix
