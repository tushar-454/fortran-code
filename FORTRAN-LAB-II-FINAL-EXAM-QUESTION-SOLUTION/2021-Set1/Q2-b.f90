PROGRAM integration_compare
  IMPLICIT NONE
  INTEGER :: i, n
  REAL :: a, b, h, x, trap, simp, fx
  REAL :: pi, exact, error_trap, error_simp

  pi = 3.1415927
  a = 0.0
  b = pi / 2.0
  n = 6               ! Number of subintervals (multiple of 3 for 3/8 rule)
  h = (b - a) / n

  ! Function to integrate: f(x) = exp(sin(x))
  trap = 0.0
  DO i = 1, n-1
    x = a + i*h
    trap = trap + EXP(SIN(x))
  END DO
  trap = h/2.0 * (EXP(SIN(a)) + 2*trap + EXP(SIN(b)))

  ! Simpson's 3/8 Rule
  simp = EXP(SIN(a)) + EXP(SIN(b))
  DO i = 1, n-1
    x = a + i*h
    IF (MOD(i,3) == 0) THEN
      simp = simp + 2.0 * EXP(SIN(x))
    ELSE
      simp = simp + 3.0 * EXP(SIN(x))
    END IF
  END DO
  simp = 3.0*h/8.0 * simp

  ! Exact value using high-precision or approximation
  exact = 1.71828  ! Approximate true value

  ! Errors
  error_trap = ABS(trap - exact)
  error_simp = ABS(simp - exact)

  PRINT *, 'Trapezoidal result =', trap
  PRINT *, 'Simpson 3/8 result =', simp
  PRINT *, 'Exact value =', exact
  PRINT *, 'Error (Trapezoidal) =', error_trap
  PRINT *, 'Error (Simpson 3/8) =', error_simp

END PROGRAM integration_compare
