#define MAT33_TRANSPOSE(_M,_MT) \
   _MT(1,1) = _M(1,1);\
   _MT(1,2) = _M(2,1);\
   _MT(1,3) = _M(3,1);\
   _MT(2,1) = _M(1,2);\
   _MT(2,2) = _M(2,2);\
   _MT(2,3) = _M(3,2);\
   _MT(3,1) = _M(1,3);\
   _MT(3,2) = _M(2,3);\
   _MT(3,3) = _M(3,3)

#define MAT33_VPROD(_M,_V,_VR) \
   _VR(1) = _M(1,1)*_V(1) + _M(1,2)*_V(2) + _M(1,3)*_V(3);\
   _VR(2) = _M(2,1)*_V(1) + _M(2,2)*_V(2) + _M(2,3)*_V(3);\
   _VR(3) = _M(3,1)*_V(1) + _M(3,2)*_V(2) + _M(3,3)*_V(3)

#define VEC3_CROSS(_v1,_v2,_vr) \
   _vr(1) = _v1(2)*_v2(3) - _v1(3)*_v2(2);\
   _vr(2) = _v1(3)*_v2(1) - _v1(1)*_v2(3);\
   _vr(3) = _v1(1)*_v2(2) - _v1(2)*_v2(1)


program func_vs_sub
   use math33
   implicit none

   real(8) :: A(3,3), B(3, 3), x(3), y(3), t0, t1, t_total
   real(8) :: v1(3), v2(3), w1(3), w2(3)
   integer :: i, j, k
   integer, parameter :: N = 1000000

   A = reshape([1, 2, 3, 4, 5, 6, 7, 8, 9], shape(A))
   x = [1, 2, 3]

   print*, matmul(A, x)

   t_total = 0.0
   do k = 1,N
      do j = 1,3
         do i = 1,3
            call random_number(A(i,j))
         end do
         call random_number(x(j))
      end do
      call cpu_time(t0)
      y = mat33_vecprod_fun(A, x)
      B = mat33_transpose_fun(A)
      call cpu_time(t1)
      t_total = t_total + t1 - t0
   end do

   print*, "Elapsed time (fun): ", t_total/N

   t_total = 0.0
   do k = 1,N
      do j = 1,3
         do i = 1,3
            call random_number(A(i,j))
         end do
         call random_number(x(j))
      end do
      call cpu_time(t0)
      call mat33_vecprod_sub(A, x, y)
      call mat33_transpose_sub(A, B)
      call cpu_time(t1)
      t_total = t_total + t1 - t0
   end do
   print*, "Elapsed time (sub): ", t_total/N

   t_total = 0.0
   do k = 1,N
      do j = 1,3
         do i = 1,3
            call random_number(A(i,j))
         end do
         call random_number(x(j))
      end do
      call cpu_time(t0)
      MAT33_VPROD(A, x, y)
      MAT33_TRANSPOSE(A, B)
      call cpu_time(t1)
      t_total = t_total + t1 - t0
   end do
   print*, "Elapsed time (mac): ", t_total/N

   t_total = 0.0
   do k = 1,N
      do j = 1,3
         do i = 1,3
            call random_number(A(i,j))
         end do
         call random_number(x(j))
      end do
      call cpu_time(t0)
      y = matmul(A, x)
      B = transpose(A)
      call cpu_time(t1)
      t_total = t_total + t1 - t0
   end do
   print*, "Elapsed time (int): ", t_total/N

   print*, "================================================================="

   do i = 1,3
      call random_number(v1(i))
      call random_number(v2(i))
   end do

   call vec3_cross_product(v1, v2, w1)
   VEC3_CROSS(v1, v2, w2)

   print*, "v1 = ", v1
   print*, "v2 = ", v2
   print*, "w1 = ", w1
   print*, "w2 = ", w2
   print*, "norm(w1-w2) = ", norm2(w1-w2)


end program func_vs_sub
