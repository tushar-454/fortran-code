! Consider the quadratic y = 2x^3 + 4x^2 - 5. Write a Fortran program to finds y for values of x from -3 to 3 in steps 0.2.

PROGRAM QUADRATIC_EQUATION
    IMPLICIT NONE
    REAL :: X, Y, Low, High, Incre

        PRINT*, "Enter the Lower limit:"
        READ*, Low
        PRINT*, "Enter the Higher limit:"
        READ*, High
        PRINT*, "Enter the Incremental Value:"
        READ*, Incre

    DO X = Low, High, Incre
        Y = 2.0 * X**3 + 4.0 * X**2 - 5.0
        PRINT*, "For X=",X,"Y will be", Y
    END DO

END PROGRAM QUADRATIC_EQUATION
