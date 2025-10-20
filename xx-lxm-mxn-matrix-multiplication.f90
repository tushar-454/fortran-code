PROGRAM matrix_multiplication
  IMPLICIT NONE
  INTEGER :: i, j, k, l, m, n
  REAL(8), ALLOCATABLE :: A(:, :), B(:, :), C(:, :)

  ! Input matrix dimensions
  PRINT *, "Enter the dimensions of the matrices (l):"
  READ *, l
  PRINT *, "Enter the dimensions of the matrices (m):"
  READ *, m
  PRINT *, "Enter the dimensions of the matrices (n):"
  READ *, n

  ! Allocate matrices
  ALLOCATE(A(l, m), B(m, n), C(l, n))

  ! Input matrix A
  PRINT *, REPEAT("-", 60)
  PRINT *, "Enter the elements of matrix A:"
  DO i = 1, l
    DO j = 1, m
      PRINT *, "Enter the elements of matrix A (", i, "x", j, "):"
      READ *, A(i, j)
    END DO
  END DO

  ! Input matrix B
  PRINT *, REPEAT("-", 60)
  PRINT *, "Enter the elements of matrix B:"
  DO i = 1, m
    DO j = 1, n
      PRINT *, "Enter the elements of matrix B (", i, "x", j, "):"
      READ *, B(i, j)
    END DO
  END DO

  ! Initialize matrix C
  C = 0.0d0

  ! Matrix multiplication
  DO i = 1, l
    DO j = 1, n
      DO k = 1, m
        C(i, j) = C(i, j) + A(i, k) * B(k, j)
      END DO
    END DO
  END DO
  ! Matrix multiplication shortcut => C = MATMUL(A, B)

  ! Display result
  PRINT *, ""
  PRINT *, REPEAT("=", 60)
  PRINT *, "Matrix Multiplication Result:"
  PRINT *, REPEAT("=", 60)
  PRINT *, "Resultant matrix A (", l, "x", m, "):"
  DO i = 1, l
    PRINT "(10F12.2)", A(i, :)
  END DO

  print*, repeat("=", 60)
  print *, "Resultant matrix B (", m, "x", n, "):"
  do i = 1, m
    print "(10F12.2)", B(i, :)
  end do

  print*, repeat("=", 60)
  print *, "Resultant matrix C (", l, "x", n, "):"
  do i = 1, l
    print "(10F12.2)", C(i, :)
  end do

  ! Deallocate matrices
  DEALLOCATE(A, B, C)

END PROGRAM matrix_multiplication
