PROGRAM lagrange_interp
  INTEGER :: i, j, n
  REAL :: x(5), y(5), xp, yp, term

  n = 5
  xp = 1.1

  ! Data points
  x(1) = 1.0
  x(2) = 1.3
  x(3) = 1.5
  x(4) = 1.9
  x(5) = 2.2

  ! Compute corresponding y = cos(log(x))
  DO i = 1, n
    y(i) = COS(LOG(x(i)))
  END DO

  ! Lagrange interpolation
  yp = 0.0
  DO i = 1, n
    term = y(i)
    DO j = 1, n
      IF (j /= i) THEN
        term = term * (xp - x(j)) / (x(i) - x(j))
      END IF
    END DO
    yp = yp + term
  END DO

  PRINT *, 'Interpolated value f(1.1) =', yp
  PRINT *, 'Exact value f(1.1) =', COS(LOG(1.1))
END PROGRAM lagrange_interp
