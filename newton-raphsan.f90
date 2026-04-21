PROGRAM NEWTON_RAPHSON
    IMPLICIT NONE

    REAL :: X0, X1, FX, FDX, TOL
    INTEGER :: N

    TOL = 0.00001

    PRINT*, "ENTER INITIAL GUESS X0:"
    READ*, X0

    N = 1

    PRINT*, "------------------------------"
    WRITE(*, 100)
100 FORMAT("   N", 6X, "X")
    PRINT*, "------------------------------"

    DO
        FX  = COS(X0) - X0 * EXP(X0)
        FDX = -SIN(X0) - EXP(X0) - X0 * EXP(X0)

        IF (FX == 0.0) EXIT

        IF (FDX == 0.0) THEN
            PRINT*, "DERIVATIVE IS ZERO. METHOD FAILS."
            STOP
        END IF

        WRITE(*, 200) N, X0
200     FORMAT(3X, I3, 2X, F10.6)

        X1 = X0 - FX / FDX

        IF (ABS(X1 - X0) < TOL) THEN
            X0 = X1
            EXIT
        END IF

        N  = N + 1
        X0 = X1
    END DO

    PRINT*, "------------------------------"
    WRITE(*, 300) X0
300 FORMAT("THE SOLUTION X = ", F10.6)

END PROGRAM NEWTON_RAPHSON
