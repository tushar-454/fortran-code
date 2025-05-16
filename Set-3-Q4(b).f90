program Simpsons_ThreeEighth_simple
    implicit none
    integer :: i, n
    real :: a, b, h, x, integral, exact, error, fx, val

    a = 0.0
    b = 1.5708      ! approx pi/2
    n = 6           ! multiple of 3 for 3/8 rule

    h = (b - a) / n
    integral = 0.0

    val = cos(a)
    if (val .lt. 0.0) val = 0.0
    fx = sqrt(val)
    integral = integral + fx

    val = cos(b)
    if (val .lt. 0.0) val = 0.0
    fx = sqrt(val)
    integral = integral + fx

    do i = 1, n-1
        x = a + i*h
        val = cos(x)
        if (val .lt. 0.0) val = 0.0
        fx = sqrt(val)

        if (mod(i, 3) .eq. 0) then
            integral = integral + 2.0 * fx
        else
            integral = integral + 3.0 * fx
        end if
    end do

    integral = integral * (3.0 * h / 8.0)

    exact = 1.19814          ! approximate exact value
    error = abs(integral - exact)

    print *, "Integral using Simpson's 3/8 rule: ", integral
    print *, "Exact value: ", exact
    print *, "Absolute error: ", error

end program Simpsons_ThreeEighth_simple
