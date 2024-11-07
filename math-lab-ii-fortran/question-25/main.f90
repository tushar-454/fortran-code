PROGRAM integral_calculation

  IMPLICIT NONE

  REAL*8 :: a, b, h_1_3, h_3_8, integral_simpson_1_3, integral_simpson_3_8
  INTEGER :: i, n_1_3, n_3_8, error_flag_1_3, error_flag_3_8

  ! Input parameters
  a = 0.0
  b = 1.0

  ! Number of intervals
  n_1_3 = 100  ! Even number for Simpson's 1/3 rule
  n_3_8 = 99   ! Multiple of 3 for Simpson's 3/8 rule

  ! Step sizes
  h_1_3 = (b - a) / n_1_3
  h_3_8 = (b - a) / n_3_8

  ! Initialize integrals and error flags
  integral_simpson_1_3 = 0.0
  integral_simpson_3_8 = 0.0
  error_flag_1_3 = 0
  error_flag_3_8 = 0

  ! Simpson's 1/3 rule (n_1_3 must be even)
  IF (MOD(n_1_3, 2) .NE. 0) THEN
    PRINT *, "Error: n_1_3 must be even for Simpson's 1/3 rule"
    error_flag_1_3 = 1
  ELSE
    integral_simpson_1_3 = h_1_3/3.0 * (f(a) + 4.0*f(a+h_1_3) + 2.0*f(a+2*h_1_3))
    DO i = 2, n_1_3-2, 2
      integral_simpson_1_3 = integral_simpson_1_3 + h_1_3/3.0 * (2.0*f(a+i*h_1_3) + 4.0*f(a+(i+1)*h_1_3))
    END DO
    integral_simpson_1_3 = integral_simpson_1_3 + h_1_3/3.0 * f(b)
  END IF

  ! Simpson's 3/8 rule (n_3_8 must be a multiple of 3)
  IF (MOD(n_3_8, 3) .NE. 0) THEN
    PRINT *, "Error: n_3_8 must be a multiple of 3 for Simpson's 3/8 rule"
    error_flag_3_8 = 1
  ELSE
    integral_simpson_3_8 = 3.0*h_3_8/8.0 * (f(a) + 3.0*f(a+h_3_8) + 3.0*f(a+2*h_3_8) + 2.0*f(a+3*h_3_8))
    DO i = 3, n_3_8-3, 3
      integral_simpson_3_8 = integral_simpson_3_8 + 3.0*h_3_8/8.0 * (2.0*f(a+i*h_3_8) + 3.0*f(a+(i+1)*h_3_8) + 3.0*f(a+(i+2)*h_3_8))
    END DO
    integral_simpson_3_8 = integral_simpson_3_8 + 3.0*h_3_8/8.0 * (f(b-2*h_3_8) + 3.0*f(b-h_3_8) + f(b))
  END IF

  ! Print results based on error flags
  IF (error_flag_1_3 .EQ. 0) THEN
    PRINT *, "Integral using Simpson's 1/3 rule: ", integral_simpson_1_3
  END IF

  IF (error_flag_3_8 .EQ. 0) THEN
    PRINT *, "Integral using Simpson's 3/8 rule: ", integral_simpson_3_8
  END IF

  IF (error_flag_1_3 .EQ. 1 .AND. error_flag_3_8 .EQ. 1) THEN
    PRINT *, "Both Simpson's 1/3 and 3/8 rules cannot be used. Check n values."
  END IF

CONTAINS

  ! Function to calculate the integrand
  REAL*8 FUNCTION f(x)
    IMPLICIT NONE
    REAL*8, INTENT(IN) :: x
    f = SQRT(1.0 - x*x)  ! Replace with your desired function
  END FUNCTION f

END PROGRAM integral_calculation
