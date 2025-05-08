! Program to calculate compound interest for each year up to 10 years
! Principal amount: 100000
! Annual interest rate: 5%
! Time period: 10 years

program CompoundInterest
    implicit none
    integer :: year
    real :: principal, rate, amount

    principal = 100000.0   ! Initial deposit
    rate = 0.05            ! Annual interest rate (5%)

    print *, 'Year', '    ', 'Amount (Taka)'
    print *, '-----------------------'

    do year = 1, 10
        ! Calculate compound amount for each year
        amount = principal * (1.0 + rate) ** year
        print '(I4, 2X, F12.2)', year, amount
    end do

end program CompoundInterest
