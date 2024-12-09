module load compiler/intel/oneapi-latest
# -ipo produces faster code and allows inlinng
ifort -ipo -fpp -O3 matrix3.f90 matrix.f90 main.f90 -c
ifort -ipo -fpp -O3 matrix3.o   matrix.o   main.o   -o ifort.o

rm matrix3.o matrix.o main.o
