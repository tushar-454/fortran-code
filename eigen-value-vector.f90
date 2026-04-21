PROGRAM POWER_METHOD
    IMPLICIT NONE

    INTEGER, PARAMETER :: N = 3
    REAL :: A(N,N), V(N), V_NEW(N), LAMBDA, LAMBDA_OLD, NORM
    INTEGER :: I, J, ITER, MAX_ITER

    MAX_ITER = 1000

    A(1,:) = [1.0, 1.0, 0.0]
    A(2,:) = [6.0, 2.0, 0.0]
    A(3,:) = [1.0, 0.0, 3.0]

    V = [1.0, 0.0, 0.0]
    LAMBDA = 0.0

    PRINT*, "================================"
    PRINT*, "       POWER METHOD"
    PRINT*, "================================"
    WRITE(*, 100)
100 FORMAT("  ITER", 4X, "EIGENVALUE")
    PRINT*, "--------------------------------"

    DO ITER = 1, MAX_ITER

        ! MATRIX-VECTOR MULTIPLY: V_NEW = A * V
        DO I = 1, N
            V_NEW(I) = 0.0
            DO J = 1, N
                V_NEW(I) = V_NEW(I) + A(I,J) * V(J)
            END DO
        END DO

        ! FIND LARGEST COMPONENT (EIGENVALUE APPROX)
        LAMBDA_OLD = LAMBDA
        LAMBDA = V_NEW(1)
        DO I = 2, N
            IF (ABS(V_NEW(I)) > ABS(LAMBDA)) LAMBDA = V_NEW(I)
        END DO

        ! NORMALIZE EIGENVECTOR
        V = V_NEW / LAMBDA

        WRITE(*, 200) ITER, LAMBDA
200     FORMAT(3X, I4, 4X, F12.6)

        ! CONVERGENCE CHECK
        IF (ABS(LAMBDA - LAMBDA_OLD) < 0.00001) EXIT

    END DO

    PRINT*, "================================"
    WRITE(*, 300) LAMBDA
300 FORMAT("  LARGEST EIGENVALUE  = ", F12.6)
    PRINT*, "  EIGENVECTOR:"
    DO I = 1, N
        WRITE(*, 400) I, V(I)
400     FORMAT("  V(", I1, ") = ", F12.6)
    END DO
    PRINT*, "================================"

END PROGRAM POWER_METHOD
