      PROGRAM LU_FACTORIZATION
      IMPLICIT NONE
      REAL A(3,3), L(3,3), U(3,3), B(3), Y(3), X(3)
      INTEGER I, J
      
!     COEFFICIENT MATRIX (ROW-WISE)
      A(1,1) = 2.0;  A(1,2) = 4.0;  A(1,3) = 6.0
      A(2,1) = 3.0;  A(2,2) = 8.0;  A(2,3) = 5.0
      A(3,1) = -1.0; A(3,2) = 1.0;  A(3,3) = 2.0
!     USING RESHAPE
      ! A = RESHAPE( (/ 2.0, 3.0, -1.0, &
      !                4.0, 8.0, 1.0, &
      !               6.0, 5.0, 2.0 /), &
      !               SHAPE(A) )
      
!     RIGHT-HAND SIDE
      B(1) = 22.0
      B(2) = 27.0
      B(3) = 2.0
      
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
         PRINT '(3F10.5)', L(I,:)
      END DO
      PRINT*

!     OUTPUT U MATRIX
      PRINT*, 'U MATRIX:'
      DO I = 1, 3
         PRINT '(3F10.5)', U(I,:)
      END DO
      PRINT*

!     OUTPUT SOLUTION
      PRINT*, 'SOLUTION (CORRECT TO 5 DECIMAL PLACES):'
      PRINT '(A,F10.5)', ' X =', X(1)
      PRINT '(A,F10.5)', ' Y =', X(2)
      PRINT '(A,F10.5)', ' Z =', X(3)
      
      END PROGRAM LU_FACTORIZATION
