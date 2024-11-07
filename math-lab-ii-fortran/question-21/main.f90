!Use Gaussian elimination method to solve the following system of linear equation
! x1 +3x2+8x3 =4
! x1+4x2+3x3 =- 2
! x1+3x2+4x3 =1

PROGRAM SOLVE_LINEAR_SYSTEM
    IMPLICIT NONE
    REAL :: A(3,4), X(3)
    INTEGER :: I, J
    REAL :: FACTOR

    DO I = 1, 3
        DO J = 1, 4
            PRINT*, "Enter the value for",I,",",J
            READ*, A(I,J)
        END DO
    END DO

    DO I = 1, 2
        DO J = I+1, 3
            FACTOR = A(J, I) / A(I, I)
            A(J, I:4) = A(J, I:4) - FACTOR * A(I, I:4)
        END DO
    END DO

    X(3) = A(3, 4) / A(3, 3)
    X(2) = (A(2, 4) - A(2, 3) * X(3)) / A(2, 2)
    X(1) = (A(1, 4) - A(1, 3) * X(3) - A(1, 2) * X(2)) / A(1, 1)

    PRINT *, "Solution:"
    PRINT *, "X1 = ", X(1)
    PRINT *, "X2 = ", X(2)
    PRINT *, "X3 = ", X(3)
END PROGRAM SOLVE_LINEAR_SYSTEM


