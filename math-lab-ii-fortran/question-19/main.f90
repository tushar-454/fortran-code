!Suppose an amount of Tk.5000.00 is deposited in a saving account in 2000 and
!suppose the bank pays 9 percent interest on the account compounded annually.
!Write a Fortran program which prints the YEAR and the AMOUNT of the
!account until 2020.

PROGRAM SAVINGS_ACCOUNT
    IMPLICIT NONE
    INTEGER :: I, START_YEAR, END_YEAR
    REAL :: INTEREST, AMOUNT

    PRINT*, 'Enter the starting year:'
    READ*, START_YEAR
    PRINT*, 'Enter the ending year:'
    READ*, END_YEAR
    PRINT*, 'Enter the initial amount:'
    READ*, AMOUNT
    PRINT*, 'Enter the interest (%):'
    READ*, INTEREST

    DO I = START_YEAR, END_YEAR
        AMOUNT = AMOUNT * (1 + (INTEREST/100.0))
        PRINT*, "Year:", I, "-", " Total amount is:", AMOUNT
    END DO

END PROGRAM SAVINGS_ACCOUNT
