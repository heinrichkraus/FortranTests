module load compiler/intel/oneapi-latest

ifort -fpp -c -finline -O3 -xHost -ipo -fp-model strict matrix3.f90 matrix.f90 main.f90
ifort -fpp -finline -O3 -xHost -ipo -fp-model strict matrix3.o matrix.o main.o -o ifort.o

rm matrix3.o matrix.o main.o
