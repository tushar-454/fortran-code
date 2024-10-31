! PRINT Matrix A+A^t

PROGRAM MATRIX_SUM_WITH_TRANSPOSE
  IMPLICIT NONE
  INTEGER :: N, I, J
  REAL :: A(100, 100), AT(100, 100), SUM(100, 100)

  PRINT *, "Enter the size of the square matrix:"
  READ *, N

  PRINT *, "Enter the matrix elements:"
  DO I = 1, N
    DO J = 1, N
      PRINT *, "Enter the value for", I, "x", J
      READ *, A(I, J)
    END DO
  END DO

  DO I = 1, N
    DO J = 1, N
      AT(J, I) = A(I, J)
    END DO
  END DO

  DO I = 1, N
    DO J = 1, N
      SUM(I, J) = A(I, J) + AT(I, J)
    END DO
  END DO

  PRINT *, "The sum of A and its transpose matrix is:"
  DO I = 1, N
    PRINT *, (SUM(I, J), J = 1, N)
  END DO

END PROGRAM MATRIX_SUM_WITH_TRANSPOSE
