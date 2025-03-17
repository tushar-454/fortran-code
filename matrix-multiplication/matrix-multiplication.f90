PROGRAM MAIN
DIMENSION A(4,5), B(5,6), C(4,6)
PRINT*, 'Enter elements of matrix A:'
READ*, ((A(I,J), J=1,5), I=1,4)
PRINT*, 'Enter elements of matrix B:'
READ*, ((B(I,J), J=1,6), I=1,5)
PRINT*, 'Product of matrices A and B:'
DO I=1,4
  DO J=1,6
    C(I,J) = 0
    DO K=1,5
      C(I,J) = C(I,J) + A(I,K)*B(K,J)
    END DO
  END DO
END DO
DO I=1,4
  PRINT*, (C(I,J), J=1,6)
END DO
END PROGRAM MAIN