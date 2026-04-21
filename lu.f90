PROGRAM LU_FACTORIZATION
      IMPLICIT NONE

      REAL A(3,3), L(3,3), U(3,3), B(3), Y(3), X(3)
      INTEGER I, J

    A(1,:) = [2.0,  4.0, 6.0]
    A(2,:) = [3.0,  8.0, 5.0]
    A(3,:) = [-1.0, 1.0, 2.0]
    B      = [22.0, 27.0, 2.0]

!     INITIALIZE L AND U
      DO I = 1, 3
         DO J = 1, 3
            L(I,J) = 0.0
            U(I,J) = 0.0
         END DO
      END DO

!     LU FACTORIZATION
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

!     OUTPUT SOLUTION
      PRINT*, 'SOLUTION (CORRECT TO 5 DECIMAL PLACES):'
      WRITE (*,30) X(1)
30    FORMAT("X=", F10.5)
      WRITE (*,40) X(2)
40    FORMAT("Y=", F10.5)
      WRITE (*,50) X(3)
50    FORMAT("Z=", F10.5)

      END PROGRAM LU_FACTORIZATION
