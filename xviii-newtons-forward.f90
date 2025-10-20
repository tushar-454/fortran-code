PROGRAM newtons_forward_interpolation
  IMPLICIT NONE
  INTEGER :: n, i, j
  REAL(8) :: x(0:15), y(0:15, 0:15)
  REAL(8) :: x_interp, u, sum_result, p_term

  ! Input: number of data points
  PRINT *, "Enter the number of data points (n, where n <= 15):"
  READ *, n
  PRINT *, REPEAT("-", 60)
  ! Input: x values
  DO i = 0, n - 1
    PRINT *, "Enter x(", i, "):"
    READ *, x(i)
  END DO
  PRINT *, REPEAT("-", 60)
  ! Input: y values
  DO i = 0, n - 1
    PRINT *, "Enter y(", i, "):"
    READ *, y(0, i)
  END DO
  PRINT *, REPEAT("-", 60)
  ! Input: interpolation point
  PRINT *, "Enter the value of x for interpolation:"
  READ *, x_interp

  ! Calculate forward difference table
  DO i = 1, n - 1
    DO j = 0, n - i - 1
      y(i, j) = y(i - 1, j + 1) - y(i - 1, j)
    END DO
  END DO

  ! Display forward difference table
  PRINT *, ""
  PRINT *, "Forward Difference Table:"
  PRINT *, REPEAT("=", 60)
  DO i = 0, n - 1
    PRINT "(A, I2, A, 10F10.4)", "Order ", i, ": ", (y(i, j), j = 0, n - i - 1)
  END DO

  ! Calculate u value
  u = (x_interp - x(0)) / (x(1) - x(0))

  ! Newton's forward interpolation formula
  sum_result = y(0, 0)
  p_term = 1.0d0

  DO i = 1, n - 1
    p_term = p_term * (u - REAL(i - 1, 8)) / REAL(i, 8)
    sum_result = sum_result + p_term * y(i, 0)
  END DO

  ! Output result
  PRINT *, ""
  PRINT *, REPEAT("=", 60)
  PRINT *, ""
  PRINT "(A, F8.1, A, F12.5)", " Interpolated value of F(", x_interp, ") = ", sum_result
  PRINT *, ""
  PRINT *, REPEAT("=", 60)

END PROGRAM newtons_forward_interpolation