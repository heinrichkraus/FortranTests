using BenchmarkTools
using LinearAlgebra

# export LD_LIBRARY_PATH=$PWD
# gfortran -fpic -c kslinalg.f90
# gfortran -shared -o kslinalg.so kslinalg.o

# function F available as :__kslinalg_MOD_F in :kslnalg

const lib = Libc.dlopen("kslinalg.so")
fdot = Libc.dlsym(lib, :__kslinalg_MOD_ksdot)

function fortran_dot(x::Vector{Float64}, y::Vector{Float64})
    length(x) != length(y) && error("Vectors x and y have to be of same length!")
    n = length(x)

    res = ccall(
        fdot,
        Float64,
        (Ptr{Float64}, Ptr{Float64}, Ref{Int32}),
        x, y, n
    )
    return res
end

function julia_dot(x::Vector{Float64}, y::Vector{Float64})
    length(x) != length(y) && error("Vectors x and y have to be of same length!")
    n = length(x)

    res = 0.0
    for i = 1:n
        res += x[i] * y[i]
    end

    return res
end


function main(n)
    x = rand(n)
    y = rand(n)

    @btime fortran_dot($x, $y)
    @btime julia_dot($x, $y)

    println(fortran_dot(x, y) - julia_dot(x, y))
end

main(100)