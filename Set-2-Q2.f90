! Program to check if a matrix is symmetric or anti-symmetric and print C = A^2 + A, D = A^2 * A
program matrix_properties
    implicit none
    integer, parameter :: n = 3
    integer :: i, j, k
    logical :: is_symmetric, is_antisymmetric
    real :: A(n, n), AT(n, n), C(n, n), D(n, n), A2(n, n)

    ! Initialize matrix A
    A = reshape([0.0, 8.0, -4.0, &
                -8.0, 0.0, 3.0, &
                 4.0, -3.0, 0.0], [n, n])
    
    ! if take input from user
    ! print *, 'Enter the elements of the matrix A: (row wise)'
    ! read *, ((A(i, j), j = 1, n), i = 1, n)


    ! Compute transpose of A
    do i = 1, n
        do j = 1, n
            AT(i, j) = A(j, i)
        end do
    end do
    ! shortcut for transpose using intrinsic function
    ! AT = transpose(A)

    ! Check for symmetry and anti-symmetry using .NE. for logical comparison
    is_symmetric = .true.
    is_antisymmetric = .true.
    do i = 1, n
        do j = 1, n
            if (A(i, j) .NE. AT(i, j)) is_symmetric = .false.
            if (A(i, j) .NE. -AT(i, j)) is_antisymmetric = .false.
        end do
    end do

    ! Print results
    if (is_symmetric) then
        print *, 'Matrix A is symmetric.'
    else
        print *, 'Matrix A is NOT symmetric.'
    end if
    if (is_antisymmetric) then
        print *, 'Matrix A is anti-symmetric.'
    else
        print *, 'Matrix A is NOT anti-symmetric.'
    end if

    ! Compute A^2 = A * A
    A2 = 0.0
    do i = 1, n
        do j = 1, n
            do k = 1, n
                A2(i, j) = A2(i, j) + A(i, k) * A(k, j)
            end do
        end do
    end do
    ! shortcut for matrix multiplication using intrinsic function
    ! A2 = matmul(A, A)

    ! Compute C = A^2 + A
    C = A2 + A

    ! Compute D = A^2 * A
    D = 0.0
    do i = 1, n
        do j = 1, n
            do k = 1, n
                D(i, j) = D(i, j) + A2(i, k) * A(k, j)
            end do
        end do
    end do
    ! shortcut for matrix multiplication using intrinsic function
    ! D = matmul(A2, A)

    ! Print matrix C
    print *, 'Matrix C = A^2 + A:'
    do i = 1, n
        print*, C(i, :)
    end do

    ! Print matrix D
    print *, 'Matrix D = A^2 * A:'
    do i = 1, n
        print *, (D(i, j), j = 1, n)
    end do

end program matrix_properties
