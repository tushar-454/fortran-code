! Program to calculate loan repayment with monthly interest and fixed payment
! Problem: Set-4-Q2 (b) and (c)

program LoanRepayment
    implicit none
    real :: principal, interest_rate, payment, owed, interest
    integer :: month, total_months
    real :: last_payment

    ! Initialize variables
    principal = 300.0         ! Initial loan amount (Tk. 300)
    interest_rate = 0.015     ! Monthly interest rate (1.5%)
    payment = 25.0            ! Monthly payment (Tk. 25)
    owed = principal

    print *, 'Month', 'Amount Owed (Tk)'
    print *, '------------------------'

    ! (b) Print the amount owed each month for a year
    do month = 1, 12
        interest = owed * interest_rate
        owed = owed + interest - payment
        if (owed .LT. 0.0) owed = 0.0
        print '(I3, 2X, F10.2)', month, owed
        if (owed == 0.0) exit
    end do

    ! (c) Find the number of months to repay the loan and last installment
    owed = principal
    total_months = 0
    do while (owed > 0.0)
        interest = owed * interest_rate
        owed = owed + interest
        total_months = total_months + 1
        if (owed .LE. payment) then
            last_payment = owed
            owed = 0.0
        else
            owed = owed - payment
        end if
    end do

    print *
    print *, 'Total months to repay loan:', total_months
    print *, 'Last installment amount (Tk):', last_payment

end program LoanRepayment
