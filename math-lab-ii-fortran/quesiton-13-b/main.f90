! Sums the elements above the main diagonal of A

PROGRAM SUM_UPPER_DIAGONAL
  DIMENSION A(100, 100)

  PRINT *, "Enter the size of the Square Matrix:"
  READ *, N

  PRINT *, "Enter the matrix elements:"
  DO I = 1, N
    DO J = 1, N
      PRINT *, "Enter the value for:", I, "X", J
      READ *, A(I, J)
    END DO
  END DO

  SUM = 0.0

  PRINT *, "The matrix is:"
  DO I = 1, N
    PRINT *, (A(I, J), J = 1, N)
  END DO

  DO I = 1, N
    DO J = 1, N
      IF (I .LT. J) THEN
        SUM = SUM + A(I, J)
      END IF
    END DO
  END DO

  PRINT *, "The sum of the upper diagonal elements is:", SUM

END PROGRAM SUM_UPPER_DIAGONAL
