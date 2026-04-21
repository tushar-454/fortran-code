PROGRAM SOR_METHOD
    IMPLICIT NONE
    INTEGER, PARAMETER :: N = 3
    INTEGER :: MAX_ITER
    REAL :: A(N,N), B(N), X(N), TOL

!===== DIAGONALLY DOMINANT MATRIX (solution: X=3, Y=1, Z=2) =====
    A(1,:) = [10.0, 2.0,  1.0]
    A(2,:) = [1.0,  10.0, 2.0]
    A(3,:) = [1.0,  1.0,  10.0]
    B      = [34.0, 17.0, 24.0]

    TOL = 0.00001
    MAX_ITER = 1000

!===== RUN DIFFERENT OMEGA VALUES =====
    CALL SOLVE_SOR(A, B, X, 0.9, TOL, MAX_ITER, N)
    CALL SOLVE_SOR(A, B, X, 1.0, TOL, MAX_ITER, N)
    CALL SOLVE_SOR(A, B, X, 1.9, TOL, MAX_ITER, N)

END PROGRAM SOR_METHOD

!================ SOR SUBROUTINE =================
SUBROUTINE SOLVE_SOR(A, B, X, OMEGA, TOL, MAX_ITER, N)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: N, MAX_ITER
    REAL, INTENT(IN) :: A(N,N), B(N), OMEGA, TOL
    REAL, INTENT(OUT) :: X(N)
    REAL :: X_OLD(N), SUM1
    INTEGER :: I, J, ITER
    LOGICAL :: CONVERGED

    X = 0.0
    CONVERGED = .FALSE.

!===== HEADER =====
    PRINT*, "--------------------------------"
    PRINT*, "OMEGA =", OMEGA

!===== ITERATION =====
    DO ITER = 1, MAX_ITER
        X_OLD = X
        DO I = 1, N
            SUM1 = 0.0
            DO J = 1, N
                IF (J .NE. I) SUM1 = SUM1 + A(I,J) * X(J)
            END DO
            X(I) = (1.0 - OMEGA) * X_OLD(I) + &
                   OMEGA * (B(I) - SUM1) / A(I,I)
        END DO
        IF (MAXVAL(ABS(X - X_OLD)) < TOL) THEN
            CONVERGED = .TRUE.
            EXIT
        END IF
    END DO

!===== OUTPUT =====
    IF (CONVERGED) THEN
        PRINT*, "RESULT:"
        WRITE(*,100) X(1), X(2), X(3)
100     FORMAT("X = ", F10.5, "  Y = ", F10.5, "  Z = ", F10.5)
        WRITE(*,200) ITER
200     FORMAT("ITERATIONS = ", I0)
    ELSE
        PRINT*, "NOT CONVERGED"
    END IF

END SUBROUTINE SOLVE_SOR
