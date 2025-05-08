! Program to solve matrix operations as per Q3
! (a) Trace, (b) Sum above diagonal, (c) Sum below diagonal, (d) A + A^T

program matrix_operations
    implicit none
    real :: A(3, 3)
    real :: trace, sum_above, sum_below
    real :: A_plus_AT(3, 3)
    integer :: i, j

    ! Define the matrix A as given in the question
    A = reshape([1.0/3, -2.0/3, 2.0/3, &
                 2.0/3, -1.0/3, -2.0/3, &
                 2.0/3, 2.0/3, 1.0/3], [3, 3])

    ! (a) Find the trace of A (sum of diagonal elements)
    trace = 0.0
    do i = 1, 3
        trace = trace + A(i, i)
    end do
    print *, '(a) Trace of A = ', trace

    ! (b) and (c) Sum above and below the main diagonal using i and j comparison
    sum_above = 0.0
    sum_below = 0.0
    do i = 1, 3
        do j = 1, 3
            if (i < j) then
                sum_above = sum_above + A(i, j)
            else if (i > j) then
                sum_below = sum_below + A(i, j)
            end if
        end do
    end do
    print *, '(b) Sum above main diagonal = ', sum_above
    print *, '(c) Sum below main diagonal = ', sum_below

    ! (d) Print matrix A + A^T
    do i = 1, 3
        do j = 1, 3
            A_plus_AT(i, j) = A(i, j) + A(j, i)
        end do
    end do
    print *, '(d) Matrix A + A^T:'
    do i = 1, 3
        print '(3F8.3)', A_plus_AT(i, :)
    end do

end program matrix_operations
