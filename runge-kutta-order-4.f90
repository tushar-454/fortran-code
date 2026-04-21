PROGRAM RKO4
    IMPLICIT NONE

    INTEGER ITER, N
    REAL X0, Y0, X1, Y1, S1, S2, S3, S4, F, H

    ITER = 0

    PRINT*, "Enter X0, Y0"
    READ*, X0, Y0

    PRINT*, "Enter X1 (final point):"
    READ*, X1

    PRINT*, "Enter number of sub intervals"
    READ*, N

!===== Runge Kutta 4th order =========
    H = (X1 - X0) / REAL(N)

    WRITE(*,30)
30  FORMAT(5X,"X",9X,"F(X)")

40  FORMAT(1X, F10.4, F10.4)

50  S1 = F(X0, Y0)
    S2 = F(X0 + H/2.0, Y0 + S1*H/2.0)
    S3 = F(X0 + H/2.0, Y0 + S2*H/2.0)
    S4 = F(X0 + H, Y0 + S3*H)

    Y1 = Y0 + (S1 + 2.0*S2 + 2.0*S3 + S4) * H/6.0
    X1 = X0 + H

    WRITE(*,40) X1, Y1

    IF (ITER .LT. N) THEN
        ITER = ITER + 1
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
    F = X - Y**2
    RETURN
END FUNCTION
