      REAL A(3,3), L(3,3), U(3,3), B(3), Y(3), X(3)
      INTEGER I, J
    ! Coefficient matrix
        A = reshape((/1.0, 3.0, 8.0, &
                     1.0, 4.0, 3.0, &
                     1.0, 3.0, 4.0/), (/3,3/))
        ! Right-hand side
       b = (/4.0, -2.0, 1.0/)
!     INITIALIZE L AND U
      DO I = 1, 3
         DO J = 1, 3
            L(I,J) = 0.0
            U(I,J) = 0.0
         END DO
      END DO

!     LU FACTORIZATION MANUALLY
      L(1,1) = 1.0
      U(1,1) = A(1,1)
      U(1,2) = A(1,2)
      U(1,3) = A(1,3)

      L(2,1) = A(2,1)/U(1,1)
      L(2,2) = 1.0
      U(2,2) = A(2,2) - L(2,1)*U(1,2)
      U(2,3) = A(2,3) - L(2,1)*U(1,3)

      L(3,1) = A(3,1)/U(1,1)
      L(3,2) = (A(3,2) - L(3,1)*U(1,2)) / U(2,2)
      L(3,3) = 1.0
      U(3,3) = A(3,3) - L(3,1)*U(1,3) - L(3,2)*U(2,3)

!     FORWARD SUBSTITUTION: L*Y = B
      Y(1) = B(1)
      Y(2) = B(2) - L(2,1)*Y(1)
      Y(3) = B(3) - L(3,1)*Y(1) - L(3,2)*Y(2)

!     BACKWARD SUBSTITUTION: U*X = Y
      X(3) = Y(3)/U(3,3)
      X(2) = (Y(2) - U(2,3)*X(3))/U(2,2)
      X(1) = (Y(1) - U(1,2)*X(2) - U(1,3)*X(3))/U(1,1)

!     OUTPUT L MATRIX
      PRINT*, 'L MATRIX:'
      DO I = 1, 3
         PRINT*, L(I,1), L(I,2), L(I,3)
      END DO

!     OUTPUT U MATRIX
      PRINT*, 'U MATRIX:'
      DO I = 1, 3
         PRINT*, U(I,1), U(I,2), U(I,3)
      END DO

!     OUTPUT SOLUTION
      PRINT*, 'SOLUTION:'
      PRINT*, 'X =', X(1)
      PRINT*, 'Y =', X(2)
      PRINT*, 'Z =', X(3)
      END
