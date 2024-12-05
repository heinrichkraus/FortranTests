module load compiler/intel/oneapi-latest

ifort -fpp -finline -O3 -xHost -ipo -fp-model strict matrix3.f90 matrix.f90 main.f90 -o ifort.o
