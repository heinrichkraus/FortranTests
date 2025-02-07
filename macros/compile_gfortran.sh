#-flto (link time optimization) can produce better results
gfortran -cpp -ffree-line-length-none -O3 -finline-arg-packing -finline-matmul-limit=50 matrix3.f90 matrix.f90 main.f90 -c
gfortran -cpp -ffree-line-length-none -O3 -finline-arg-packing -finline-matmul-limit=50 matrix3.o   matrix.o   main.o   -o gfort.o

rm matrix3.o matrix.o main.o
