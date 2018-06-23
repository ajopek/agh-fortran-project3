module test
    contains
    subroutine run_mm(N)
    use library, only : mm

    real(kind= 8), allocatable :: first(:, :), second(:, :), result(:, :)
    real :: start, finish
    integer(kind=4) :: i, j
    integer(kind=4), intent(in) :: N
    integer :: status

    allocate(first(n, n))
    allocate(second(n, n))
    allocate(result(n, n))

    do i=1,N
        do j=1,N
            first(i, j) = i*j + 10
            second(i, j) = i**j + 1
        enddo
    enddo

    call cpu_time(start)
    call mm(first, second, result, status)
    call cpu_time(finish)
    print '(i8, f10.4)',N, finish-start

    end subroutine

    subroutine run_gauss(N)
    use library, only : gaussian_elimination

    real(kind= 8), allocatable :: A(:, :), X(:)
    real :: start, finish
    integer(kind=4) :: i
    integer(kind=4), intent(in) :: N
    real(kind = 8) :: P1, P2, h2

    allocate(A(n, n))
    allocate(X(n))

    h2 = 1.0 / real(N * N)
    P1 = 1.0 / h2
    P2 = -2.0 / h2

    A = 0
    do i = 1,N
        A(i, i) = P2
        if (i .NE. 1) then 
            A(i, i - 1) = P1 
        endif
        if (i .NE. N) then 
            A(i, i + 1) = P1 
        endif
    enddo

    X(:) = 0.0
    X(N) = 1

    call cpu_time(start)
    call gaussian_elimination(A, X, N)
    call cpu_time(finish)
    print '(i8, f10.4)',N, finish-start
    end subroutine
end module
