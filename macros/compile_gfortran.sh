gfortran -cpp -ffree-line-length-none -flto -O3 -finline-arg-packing -finline-matmul-limit=50 matrix3.f90 matrix.f90 main.f90 -o gfort.o
