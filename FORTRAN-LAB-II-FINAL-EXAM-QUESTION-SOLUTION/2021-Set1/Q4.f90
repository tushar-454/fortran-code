program triangle_type
    implicit none
    real :: a, b, c
    logical :: is_triangle, is_equilateral, is_isosceles, is_right

    ! Read input values
    print *, "Enter three positive numbers for triangle sides:"
    read *, a, b, c

    ! Check if values are positive
    if (a <= 0 .or. b <= 0 .or. c <= 0) then
        print *, "All values must be positive."
        stop
    end if

    ! Check triangle inequality
    is_triangle = (a + b > c) .and. (a + c > b) .and. (b + c > a)

    if (.not. is_triangle) then
        print *, "DOES NOT FORM A TRIANGLE"
    else
        ! Determine triangle type
        is_equilateral = (a == b) .and. (b == c)
        is_isosceles = (a == b) .or. (b == c) .or. (a == c)
        is_right = abs(a**2 + b**2 - c**2) < 1e-5 .or. &
                 abs(a**2 + c**2 - b**2) < 1e-5 .or. &
                 abs(b**2 + c**2 - a**2) < 1e-5

        if (is_equilateral) then
            print *, "EQUILATERAL TRIANGLE"
        else if (is_right) then
            print *, "RIGHT-ANGLED TRIANGLE"
        else if (is_isosceles) then
            print *, "ISOSCELES TRIANGLE"
        else
            print *, "SCALENE TRIANGLE"
        end if
    end if
end program triangle_type
