PROGRAM PICARD_METHOD
    IMPLICIT NONE

    REAL :: X, Y0, Y1, Y2, H
    INTEGER :: I, N

    Y0 = 1.0
    X  = 0.0

    PRINT*, "ENTER STEP SIZE H AND NUMBER OF STEPS N:"
    READ*, H, N

    PRINT*, "------------------------------"
    WRITE(*, 100)
100 FORMAT("   X", 6X, "Y1 (1ST)", 6X, "Y2 (2ND)")
    PRINT*, "------------------------------"

    WRITE(*, 200) X, Y0, Y0
200 FORMAT(F6.3, 3X, F10.6, 3X, F10.6)

    DO I = 1, N
        X  = I * H
        Y1 = 1.0 + X + 0.5 * X**2
        Y2 = 1.0 + X + 0.5 * X**2 + (X**3) / 6.0 + (X**4) / 24.0
        WRITE(*, 200) X, Y1, Y2
    END DO

    PRINT*, "------------------------------"

END PROGRAM PICARD_METHOD
