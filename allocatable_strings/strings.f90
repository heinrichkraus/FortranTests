program strings
    implicit none

    character(len=:), allocatable :: string
    allocate(character(64)::string)
    write(string, "(A, 2I4, A)") "I have a string", 1, 2, " FINAL"
    write(*,*) string
end program strings