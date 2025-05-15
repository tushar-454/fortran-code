program lagrange_interpolation
    implicit none
    integer, parameter :: n = 5
    real(8) :: x(n), y(n), xp, yp, exact, L
    integer :: i, j

    ! Given data points
    x = (/1.0d0, 1.3d0, 1.5d0, 1.9d0, 2.2d0/)
    do i = 1, n
        y(i) = sin(log(x(i)))
    end do

    ! Point to interpolate
    xp = 1.7d0
    yp = 0.0d0

    ! Lagrange interpolation
    do i = 1, n
        L = 1.0d0
        do j = 1, n
            if (j .NE. i) then
                L = L * (xp - x(j)) / (x(i) - x(j))
            end if
        end do
        yp = yp + L * y(i)
    end do

    ! Exact value
    exact = sin(log(xp))

    print *, 'Lagrange interpolation at x =', xp, 'is', yp
    print *, 'Exact value at x =', xp, 'is', exact
    print *, 'Absolute error =', abs(yp - exact)
end program lagrange_interpolation

