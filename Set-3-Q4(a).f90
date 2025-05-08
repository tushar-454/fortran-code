program LU_Factorization
    implicit none
    integer :: i, j, k, n
    real(8), dimension(3,3) :: A, L, U
    real(8), dimension(3) :: b, y, x

    n = 3
    ! Coefficient matrix
    A = reshape([1.0d0, 3.0d0, 8.0d0, &
                 1.0d0, 4.0d0, 3.0d0, &
                 1.0d0, 3.0d0, 4.0d0], [3,3])
    ! Right-hand side
    b = [4.0d0, -2.0d0, 1.0d0]

    ! Initialize L and U
    L = 0.0d0
    U = 0.0d0

    ! LU Decomposition (Doolittle's method)
    do i = 1, n
        L(i,i) = 1.0d0
        do j = i, n
            U(i,j) = A(i,j)
            do k = 1, i-1
                U(i,j) = U(i,j) - L(i,k)*U(k,j)
            end do
        end do
        do j = i+1, n
            L(j,i) = A(j,i)
            do k = 1, i-1
                L(j,i) = L(j,i) - L(j,k)*U(k,i)
            end do
            L(j,i) = L(j,i)/U(i,i)
        end do
    end do

    ! Forward substitution to solve Ly = b
    do i = 1, n
        y(i) = b(i)
        do k = 1, i-1
            y(i) = y(i) - L(i,k)*y(k)
        end do
    end do

    ! Backward substitution to solve Ux = y
    do i = n, 1, -1
        x(i) = y(i)
        do k = i+1, n
            x(i) = x(i) - U(i,k)*x(k)
        end do
        x(i) = x(i)/U(i,i)
    end do

    ! Print L
    print *, 'L matrix:'
    do i = 1, n
        print '(3F10.4)', L(i,:)
    end do

    ! Print U
    print *, 'U matrix:'
    do i = 1, n
        print '(3F10.4)', U(i,:)
    end do

    ! Print solution
    print *, 'Solution (x1, x2, x3):'
    print '(3F10.4)', x

end program LU_Factorization
