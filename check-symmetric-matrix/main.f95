program matrix_check
    implicit none
    integer :: n, m, i, j, is_symmetric
    real :: matrix(100,100)

    ! Input matrix dimensions
    print *, 'Enter number of rows (n):'
    read *, n
    print *, 'Enter number of columns (m):'
    read *, m

    ! Check if the matrix is square
    if (n /= m) then
        print *, 'The matrix is not square.'
        stop
    end if

    ! Input matrix elements
    print *, 'Enter the elements of the matrix:'
    do i = 1, n
        do j = 1, m
        print *, 'Enter the elements of the matrix:',i,',',j
            read *, matrix(i,j)
        end do
    end do

    ! Check if the matrix is symmetric (protosomo)
    is_symmetric = 1
    do i = 1, n
        do j = 1, i
            if (matrix(i,j) /= matrix(j,i)) then
                is_symmetric = 0
                exit
            end if
        end do
        if (is_symmetric == 0) exit
    end do

    ! Output results
    if (is_symmetric == 1) then
        print *, 'The matrix is square symmetric.'
    else
        print *, 'The matrix is square but not not symmetric.'
    end if

end program matrix_check
