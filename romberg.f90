PROGRAM ROMBERG_INTEGRATION
  IMPLICIT NONE
  REAL, PARAMETER :: A = 0.0 ! Lower limit of integration
  REAL, PARAMETER :: B = 1.0 ! Upper limit of integration
  INTEGER, PARAMETER :: MAX_ITERATIONS = 5 ! Reduced iterations for simplicity
  REAL :: R(MAX_ITERATIONS, MAX_ITERATIONS) ! Romberg table
  REAL :: H, SUM
  INTEGER :: I, J, K, N

  ! Define the function to be integrated (e.g., f(x) = x^2)
  REAL FUNCTION F(X)
    REAL, INTENT(IN) :: X
    F = X**2
  END FUNCTION F

  H = B - A
  R(1, 1) = (H / 2.0) * (F(A) + F(B)) ! Initial trapezoidal rule

  DO I = 2, MAX_ITERATIONS
    H = H / 2.0
    SUM = 0.0
    N = 2**(I - 2) ! Number of new points
    DO K = 1, N
      SUM = SUM + F(A + (2.0 * K - 1.0) * H)
    END DO
    R(I, 1) = 0.5 * R(I - 1, 1) + SUM * H

    DO J = 2, I
      R(I, J) = R(I, J - 1) + (R(I, J - 1) - R(I - 1, J - 1)) / (4.0**(J - 1) - 1.0)
    END DO
  END DO

  PRINT *, 'Approximate value of integral: ', R(MAX_ITERATIONS, MAX_ITERATIONS)

END PROGRAM ROMBERG_INTEGRATION
