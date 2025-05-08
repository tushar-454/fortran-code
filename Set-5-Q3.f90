program gaussian_elimination
    implicit none
    integer :: i, j, k, n
    real :: a(3,4), factor, sum
    real :: x(3)

    n = 3

    ! Augmented matrix for the system:
    ! x1 + 3x2 + 8x3 =  4
    ! x1 + 4x2 + 3x3 = -2
    ! x1 + 3x2 + 4x3 =  1
    a = reshape([ &
        1.0, 3.0, 8.0,  4.0, &
        1.0, 4.0, 3.0, -2.0, &
        1.0, 3.0, 4.0,  1.0  &
    ], shape(a))

    ! Forward elimination to convert to upper triangular form
    do k = 1, n-1
        do i = k+1, n
            factor = a(i,k) / a(k,k)
            do j = k, n+1
                a(i,j) = a(i,j) - factor * a(k,j)
            end do
        end do
    end do

    ! Back substitution to solve for x(n) to x(1)
    x(n) = a(n,n+1) / a(n,n)
    do i = n-1, 1, -1
        sum = 0.0
        do j = i+1, n
            sum = sum + a(i,j) * x(j)
        end do
        x(i) = (a(i,n+1) - sum) / a(i,i)
    end do

    ! Print the solution
    print *, 'Solution:'
    print *, 'x(1) = ', x(1)
    print *, 'x(2) = ', x(2)
    print *, 'x(3) = ', x(3)

end program gaussian_elimination