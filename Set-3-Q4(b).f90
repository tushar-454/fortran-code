program Simpsons_ThreeEighth
    implicit none
    integer :: i, n
    real(8) :: a, b, h, x, integral, exact, error
    real(8) :: f

    a = 0.0d0
    b = 1.5707963267948966d0   ! pi/2
    n = 6                      ! n must be a multiple of 3 for 3/8 rule

    h = (b - a) / n
    integral = f(a) + f(b)

    do i = 1, n-1
        x = a + i*h
        if (mod(i,3) == 0) then
            integral = integral + 2.0d0 * f(x)
        else
            integral = integral + 3.0d0 * f(x)
        end if
    end do

    integral = integral * (3.0d0 * h / 8.0d0)

    ! Exact value (from WolframAlpha or tables)
    exact = 1.1981402347355922d0

    error = abs(integral - exact)

    print *, 'Integral using Simpson''s 3/8 rule:', integral
    print *, 'Exact value:', exact
    print *, 'Absolute error:', error

contains
    function f(x) result(val)
        real(8), intent(in) :: x
        real(8) :: val
        val = sqrt(cos(x))
    end function f

end program Simpsons_ThreeEighth