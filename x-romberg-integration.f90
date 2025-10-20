PROGRAM Romberg_Integration
  IMPLICIT NONE
  INTEGER, PARAMETER :: max_iter = 10
  REAL(8), PARAMETER :: PI = 3.141592653589793D0
  REAL(8), PARAMETER :: exact_value = 0.0887553D0
  REAL(8) :: a, b, tol, result, error, trapezoidal
  REAL(8), DIMENSION(max_iter, max_iter) :: R
  INTEGER :: i, j, n

  ! Integration limits
  a = 0.0D0
  b = PI / 4.0D0
  tol = 1.0D-5

  PRINT *, "================================================================"
  PRINT *, "Romberg Integration Method"
  PRINT *, "================================================================"
  PRINT *, "Integral: ∫[0 to π/4] x² sin(x) dx"
  PRINT "(A,F10.7)", " Exact value: ", exact_value
  PRINT *, ""
  PRINT *, "Romberg Table:"
  PRINT *, "----------------------------------------------------------------"
  PRINT *, "  n  |  R(n,0)      R(n,1)      R(n,2)      R(n,3)      R(n,4)"
  PRINT *, "----------------------------------------------------------------"

  ! Initialize first column with trapezoidal rule
  R(1,1) = trapezoidal(a, b, 1)
  PRINT "(I4,A,F12.8)", 1, "  | ", R(1,1)

  ! Romberg iteration
  DO n = 2, max_iter
    ! Compute trapezoidal rule with 2^(n-1) intervals
    R(n,1) = trapezoidal(a, b, 2**(n-1))
    
    ! Richardson extrapolation
    DO j = 2, n
      R(n,j) = (4.0D0**(j-1) * R(n,j-1) - R(n-1,j-1)) / (4.0D0**(j-1) - 1.0D0)
    END DO
    
    ! Print row
    WRITE(*, "(I4,A)", ADVANCE='NO') 2**(n-1), "  | "
    DO j = 1, n
      WRITE(*, "(F12.8)", ADVANCE='NO') R(n,j)
    END DO
    PRINT *, ""
    
    ! Check convergence
    IF (n .GT. 1) THEN
      IF (ABS(R(n,n) - R(n-1,n-1)) .LT. tol) THEN
        result = R(n,n)
        EXIT
      END IF
    END IF
  END DO

  PRINT *, "----------------------------------------------------------------"
  PRINT *, ""
  PRINT *, "Results:"
  PRINT *, "----------------------------------------------------------------"
  PRINT "(A,F12.8)", " Approximate value: ", result
  PRINT "(A,F12.8)", " Exact value:       ", exact_value
  error = ABS(result - exact_value)
  PRINT "(A,E12.5)", " Absolute error:    ", error
  PRINT "(A,I0)", " Number of levels:  ", n
  PRINT *, "================================================================"

END PROGRAM Romberg_Integration

FUNCTION f(x) RESULT(y)
  IMPLICIT NONE
  REAL(8), INTENT(IN) :: x
  REAL(8) :: y
  y = x * x * SIN(x)
END FUNCTION f

FUNCTION trapezoidal(a, b, n) RESULT(integral)
  IMPLICIT NONE
  REAL(8), INTENT(IN) :: a, b
  INTEGER, INTENT(IN) :: n
  REAL(8) :: integral, h, sum_val, x, f
  INTEGER :: i

  h = (b - a) / REAL(n, 8)
  sum_val = 0.5D0 * (f(a) + f(b))

  DO i = 1, n-1
    x = a + i * h
    sum_val = sum_val + f(x)
  END DO

  integral = h * sum_val
END FUNCTION trapezoidal
