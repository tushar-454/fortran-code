PROGRAM SIMPSONS_INTEGRATION
      IMPLICIT NONE
      DOUBLE PRECISION :: A = 0.0D0, B = 6.0D0, EXACT, SIMP13, SIMP38
      INTEGER :: N
      
      ! Display problem information
      WRITE(*,'(A)') 'Simpson''s Rule Integration: ∫₀⁶ 1/(1+x²) dx'
      WRITE(*,'(A)') '=============================================='
      
      ! Calculate exact value: ∫ 1/(1+x²) dx = arctan(x)
      EXACT = ATAN(B) - ATAN(A)
      WRITE(*,'(A,F10.6)') 'Exact value = arctan(6) = ', EXACT
      
      ! Get number of intervals (make even for Simpson's 1/3)
      WRITE(*,'(A)',ADVANCE='NO') 'Enter number of intervals: '
      read(*,*) N
      IF (MOD(N, 2) .NE. 0) N = N + 1
      
      ! Calculate using both methods
      CALL SIMPSON_13(A, B, N, SIMP13)
      CALL SIMPSON_38(A, B, N, SIMP38)
      
      ! Display results
      WRITE(*,'(/A)') 'Results:'
      WRITE(*,'(A,F10.6)') 'Exact value:      ', EXACT
      WRITE(*,'(A,F10.6,A,E10.3)') 'Simpson 1/3:      ', SIMP13, '  Error: ', ABS(EXACT-SIMP13)
      WRITE(*,'(A,F10.6,A,E10.3)') 'Simpson 3/8:      ', SIMP38, '  Error: ', ABS(EXACT-SIMP38)
      
      IF (ABS(EXACT-SIMP13) .LT. ABS(EXACT-SIMP38)) THEN
          WRITE(*,'(/A)') 'Simpson 1/3 rule is more accurate.'
      ELSE
          WRITE(*,'(/A)') 'Simpson 3/8 rule is more accurate.'
      ENDIF
      
END PROGRAM

SUBROUTINE SIMPSON_13(A, B, N, RESULT)      
      DOUBLE PRECISION :: A, B, RESULT, H, X, INTEGRAL
      INTEGER :: N, I
      
      INTERFACE
          FUNCTION F(X) RESULT(Y)
              DOUBLE PRECISION :: X, Y
          END FUNCTION F
      END INTERFACE
      
      H = (B - A) / N
      INTEGRAL = 0.0D0
      
      ! Add first and last points
      INTEGRAL = INTEGRAL + F(A)
      INTEGRAL = INTEGRAL + F(B)
      
      ! Add interior points with appropriate weights
      DO I = 1, N-1
          X = A + I * H
          IF (MOD(I, 2) .EQ. 1) THEN
              INTEGRAL = INTEGRAL + 4.0D0 * F(X)
          ELSE
              INTEGRAL = INTEGRAL + 2.0D0 * F(X)
          ENDIF
      ENDDO
      
      RESULT = INTEGRAL * (H / 3.0D0)
END SUBROUTINE

SUBROUTINE SIMPSON_38(A, B, N, RESULT)      
      DOUBLE PRECISION :: A, B, RESULT, H, X, INTEGRAL
      INTEGER :: N, I
      
      INTERFACE
          FUNCTION F(X) RESULT(Y)
              DOUBLE PRECISION :: X, Y
          END FUNCTION F
      END INTERFACE
      
      H = (B - A) / N
      INTEGRAL = 0.0D0
      
      ! Add first and last points
      INTEGRAL = INTEGRAL + F(A)
      INTEGRAL = INTEGRAL + F(B)
      
      ! Add interior points with appropriate weights
      DO I = 1, N-1
          X = A + I * H
          IF (MOD(I, 3) .EQ. 0) THEN
              INTEGRAL = INTEGRAL + 2.0D0 * F(X)
          ELSE
              INTEGRAL = INTEGRAL + 3.0D0 * F(X)
          ENDIF
      ENDDO
      
      RESULT = INTEGRAL * (3.0D0 * H / 8.0D0)
END SUBROUTINE

FUNCTION F(X) RESULT(Y)
      DOUBLE PRECISION :: X, Y
      Y = 1.0D0 / (1.0D0 + X*X)
END FUNCTION