PROGRAM RKO2
    IMPLICIT NONE

    INTEGER INTER, N
    REAL X0, Y0, X1, Y1, K1, K2, F, H

    INTER = 0

    PRINT*, "Enter X0 and Y0:"
    READ*, X0, Y0

    PRINT*, "Enter X1 (final point):"
    READ*, X1

    PRINT*, "Enter number of sub intervals:"
    READ*, N

!===== Runge Kutta order 2 =========
    H = (X1 - X0) / REAL(N)

    WRITE(*,30)
30  FORMAT(5X,"X",9X,"F(X)")

40  FORMAT(1X, F10.4, F10.4)

50  K1 = H * F(X0, Y0)
    K2 = H * F(X0 + H, Y0 + K1)

    X1 = X0 + H
    Y1 = Y0 + (K1 + K2) / 2.0

    WRITE(*,40) X1, Y1

    INTER = INTER + 1

    IF (INTER .LT. N) THEN
        X0 = X1
        Y0 = Y1
        GOTO 50
    END IF

    PRINT*
    PRINT*, "FINAL RESULT:"
    WRITE(*,60) X1, Y1
60  FORMAT(1X, "X1=", F10.4, 4X, "Y1=", F10.4)

END PROGRAM


!===== Given function ==========
FUNCTION F(X, Y)
    REAL F, X, Y
    F = X - Y
    RETURN
END FUNCTION
