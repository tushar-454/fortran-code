! Q1- (c) - START partial sub for first 20 terms
program partial_sum
    integer :: i
    real :: term, sum
    external :: general_term
    open(unit=10, file="partial_sum.txt", status="replace")

    sum = 0.0

    do i = 1, 20
        call general_term(i, term)
        sum = sum + term
        write(10, *) "Sum of first", i, "terms=", sum
    end do
    print*, "Partial sum complete and create a file name partial_sum. Open file for output."
    close(10)
end program partial_sum

subroutine general_term(n, term)
    integer :: n
    real :: term
    term = (n**2.0)/((n+1.0)*(n+2.0))*4.0**n
end subroutine
! Q1- (c) - STOP
