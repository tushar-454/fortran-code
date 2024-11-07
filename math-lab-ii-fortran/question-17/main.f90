!Input the basic pay and compute and print the total pay (basic pay+house rent allowance).

PROGRAM CALCULATE_TOTAL_PAY

    IMPLICIT NONE
    REAL :: BASIC_PAY, TOTAL_PAY

    PRINT*, "Enter the basic pay amount:"
    READ*, BASIC_PAY

    IF(BASIC_PAY.LE.800.0) THEN
        TOTAL_PAY = BASIC_PAY + (BASIC_PAY * (55.0/100.0))
    END IF
    IF(BASIC_PAY.GE.801.0 .AND. BASIC_PAY.LE.1200.0) THEN
        TOTAL_PAY = BASIC_PAY + (BASIC_PAY * (50.0/100.0))
    END IF
    IF(BASIC_PAY.GE.1201.0 .AND. BASIC_PAY.LE.1650.0) THEN
        TOTAL_PAY = BASIC_PAY + (BASIC_PAY * (45.0/100.0))
    END IF
    IF(BASIC_PAY.GE.1651.0) THEN
        TOTAL_PAY = BASIC_PAY + (BASIC_PAY * (40.0/100.0))
    END IF

    PRINT*, "The total pay (basic pay+house rent allowance):", TOTAL_PAY

END PROGRAM CALCULATE_TOTAL_PAY
