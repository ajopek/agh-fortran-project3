program main
    use test
    implicit none
    integer(kind=4) :: l, s, n
    character(len=10) :: buffer

    call get_command_argument(1, buffer, l, s)
    read(buffer,*) n

    call run_mm(n)
end program