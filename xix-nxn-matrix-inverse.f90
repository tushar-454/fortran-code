PROGRAM matrix_inverse
  IMPLICIT NONE
  INTEGER :: n, i, j, k
  REAL(8), ALLOCATABLE :: A(:, :), inv(:, :)
  REAL(8) :: factor, temp

  ! Input matrix size
  PRINT *, "Enter the size of the matrix (n x n):"
  READ *, n

  ! Allocate matrices
  ALLOCATE(A(n, n), inv(n, n))

  ! Input matrix
  PRINT *, "Enter the elements of the", n, "x", n, "matrix (row by row):"
  DO i = 1, n
    DO j = 1, n
      PRINT *, "Enter element (", i, ",", j, "):"
      READ *, A(i, j)
    END DO
  END DO

  ! Initialize inverse matrix as identity matrix
  inv = 0.0d0
  DO i = 1, n
    inv(i, i) = 1.0d0
  END DO

  ! Display original matrix
  PRINT *, ""
  PRINT *, "Original Matrix:"
  PRINT *, REPEAT("=", 60)
  DO i = 1, n
    PRINT "(10F12.4)", A(i, :)
  END DO

  ! Gauss-Jordan elimination to find inverse
  DO k = 1, n
    ! Check for zero pivot
    IF (ABS(A(k, k)).LT.1.0d-10) THEN
      PRINT *, ""
      PRINT *, "Error: Matrix is singular (not invertible)"
      PRINT *, "Pivot element at position (", k, ",", k, ") is zero"
      STOP
    END IF

    ! Divide the pivot row by pivot element
    temp = A(k, k)
    DO j = 1, n
      A(k, j) = A(k, j) / temp
      inv(k, j) = inv(k, j) / temp
    END DO

    ! Eliminate column k in all other rows
    DO i = 1, n
      IF (i.NE.k) THEN
        factor = A(i, k)
        DO j = 1, n
          A(i, j) = A(i, j) - factor * A(k, j)
          inv(i, j) = inv(i, j) - factor * inv(k, j)
        END DO
      END IF
    END DO
  END DO

  ! Display inverse matrix
  PRINT *, ""
  PRINT *, REPEAT("=", 60)
  PRINT *, "Inverse Matrix:"
  PRINT *, REPEAT("=", 60)
  DO i = 1, n
    PRINT "(10F12.6)", inv(i, :)
  END DO

  ! Deallocate matrices
  DEALLOCATE(A, inv)

END PROGRAM matrix_inverse
