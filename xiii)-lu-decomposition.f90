PROGRAM LU_Decomposition
  IMPLICIT NONE
  INTEGER, PARAMETER :: n = 3
  REAL(8), DIMENSION(n,n) :: A, L, U
  REAL(8), DIMENSION(n) :: B, X, Y
  INTEGER :: i, j, k

  ! Initialize matrix A
  A = RESHAPE([4.0D0, 1.0D0, -1.0D0, &
               2.0D0, 7.0D0, 1.0D0, &
               1.0D0, -3.0D0, 12.0D0], [n,n], ORDER=[2,1])
  
  ! Initialize vector B
  B = [3.0D0, 19.0D0, 31.0D0]

  ! Initialize L and U
  L = 0.0D0
  U = 0.0D0

  PRINT *, "================================================================"
  PRINT *, "LU Decomposition Method"
  PRINT *, "================================================================"
  PRINT *, ""
  PRINT *, "Original Matrix A:"
  DO i = 1, n
    PRINT "(3F10.4)", A(i,:)
  END DO
  PRINT *, ""
  PRINT *, "Vector B:"
  DO i = 1, n
    PRINT "(F10.4)", B(i)
  END DO
  PRINT *, ""

  ! Perform LU Decomposition using Doolittle's method
  DO i = 1, n
    ! Upper Triangular Matrix U
    DO k = i, n
      U(i,k) = A(i,k)
      DO j = 1, i-1
        U(i,k) = U(i,k) - L(i,j) * U(j,k)
      END DO
    END DO

    ! Lower Triangular Matrix L
    L(i,i) = 1.0D0
    DO k = i+1, n
      L(k,i) = A(k,i)
      DO j = 1, i-1
        L(k,i) = L(k,i) - L(k,j) * U(j,i)
      END DO
      L(k,i) = L(k,i) / U(i,i)
    END DO
  END DO

  ! Display L and U matrices
  PRINT *, "Lower Triangular Matrix L:"
  DO i = 1, n
    PRINT "(3F10.4)", L(i,:)
  END DO
  PRINT *, ""
  
  PRINT *, "Upper Triangular Matrix U:"
  DO i = 1, n
    PRINT "(3F10.4)", U(i,:)
  END DO
  PRINT *, ""

  ! Forward substitution: Solve LY = B for Y
  DO i = 1, n
    Y(i) = B(i)
    DO j = 1, i-1
      Y(i) = Y(i) - L(i,j) * Y(j)
    END DO
    Y(i) = Y(i) / L(i,i)
  END DO

  PRINT *, "Intermediate solution Y (from LY = B):"
  PRINT "(3F10.4)", Y
  PRINT *, ""

  ! Backward substitution: Solve UX = Y for X
  DO i = n, 1, -1
    X(i) = Y(i)
    DO j = i+1, n
      X(i) = X(i) - U(i,j) * X(j)
    END DO
    X(i) = X(i) / U(i,i)
  END DO

  ! Display solution
  PRINT *, "----------------------------------------------------------------"
  PRINT *, "Solution X:"
  PRINT *, "----------------------------------------------------------------"
  PRINT "(A,F10.4)", " a = ", X(1)
  PRINT "(A,F10.4)", " b = ", X(2)
  PRINT "(A,F10.4)", " c = ", X(3)
  PRINT *, ""
END PROGRAM LU_Decomposition
