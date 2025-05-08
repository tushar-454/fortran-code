! Program to compute e^5 and e^1.5 using the sum of first five terms of the series for e^x
! e^x = 1 + x/1! + x^2/2! + x^3/3! + x^4/4! + ...

program exp_series_sum
    implicit none
    integer :: i
    real :: x, term, sum
    integer :: n_terms
    real :: factorial

    n_terms = 5  ! Number of terms to sum (up to x^4/4!)

    ! Compute e^5 using 5 terms
    x = 5.0
    sum = 1.0  ! First term is always 1
    term = 1.0
    factorial = 1.0
    do i = 1, n_terms-1
        term = term * x
        factorial = factorial * i
        sum = sum + term / factorial
    end do
    print *, 'Approximate value of e^5 using 5 terms =', sum

    ! Compute e^1.5 using 5 terms
    x = 1.5
    sum = 1.0
    term = 1.0
    factorial = 1.0
    do i = 1, n_terms-1
        term = term * x
        factorial = factorial * i
        sum = sum + term / factorial
    end do
    print *, 'Approximate value of e^1.5 using 5 terms =', sum

end program exp_series_sum
