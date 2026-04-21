PROGRAM ROMBERG_INTEGRATION
    IMPLICIT NONE

    INTEGER, PARAMETER :: N = 6
    REAL :: R(N,N)
    REAL :: A, B, H, S
    INTEGER :: I, J, K
    REAL :: F

    A = 0.0
    B = ACOS(-1.0) / 4.0

    ! FIRST COLUMN (TRAPEZOIDAL)
    R(1,1) = (B - A) * (F(A) + F(B)) / 2.0

    DO I = 2, N
        H = (B - A) / REAL(2**(I-1))
        S = 0.0
        DO K = 1, 2**(I-2)
            S = S + F(A + REAL(2*K - 1) * H)
        END DO
        R(I,1) = 0.5 * R(I-1,1) + H * S
    END DO

    ! ROMBERG EXTRAPOLATION
    DO I = 2, N
        DO J = 2, I
            R(I,J) = R(I,J-1) + (R(I,J-1) - R(I-1,J-1)) / (4.0**(J-1) - 1.0)
        END DO
    END DO

    ! PRINT TABLE
    PRINT*, "================================"
    PRINT*, "        ROMBERG TABLE"
    PRINT*, "================================"
    DO I = 1, N
        DO J = 1, I
            WRITE(*, 100, ADVANCE="NO") R(I,J)
100         FORMAT(F12.6)
        END DO
        PRINT*
    END DO

    PRINT*, "================================"
    WRITE(*, 200) R(N,N)
200 FORMAT("  FINAL APPROXIMATION = ", F12.6)
    WRITE(*, 300) 0.0887553
300 FORMAT("  EXACT VALUE         = ", F12.6)
    PRINT*, "================================"

END PROGRAM ROMBERG_INTEGRATION

FUNCTION F(X) RESULT(RES)
    IMPLICIT NONE
    REAL, INTENT(IN) :: X
    REAL :: RES
    RES = X * X * SIN(X)
END FUNCTION F
