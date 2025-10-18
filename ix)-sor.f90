PROGRAM SOR_Method
  IMPLICIT NONE
  INTEGER, PARAMETER :: n = 3
  REAL(8), DIMENSION(n, n) :: A
  REAL(8), DIMENSION(n) :: b, x
  REAL(8) :: omega, tol
  INTEGER :: max_iter

  A(1,:) = [2.0D0, 4.0D0, 6.0D0]
  A(2,:) = [3.0D0, 8.0D0, 5.0D0]
  A(3,:) = [-1.0D0, 1.0D0, 2.0D0]
  b = [22.0D0, 27.0D0, 2.0D0]

  tol = 1.0D-5
  max_iter = 1000

  CALL solve_SOR(A, b, x, 0.9D0, tol, max_iter, n)
  CALL solve_SOR(A, b, x, 1.0D0, tol, max_iter, n)
  CALL solve_SOR(A, b, x, 1.9D0, tol, max_iter, n)

END PROGRAM SOR_Method


SUBROUTINE solve_SOR(A, b, x, omega, tol, max_iter, n)
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: n, max_iter
  REAL(8), DIMENSION(n,n), INTENT(IN) :: A
  REAL(8), DIMENSION(n), INTENT(IN) :: b
  REAL(8), DIMENSION(n), INTENT(OUT) :: x
  REAL(8), INTENT(IN) :: omega, tol
  REAL(8), DIMENSION(n) :: x_old
  REAL(8) :: sum1
  INTEGER :: i, j, iter
  LOGICAL :: converged

  ! Safety check for diagonal zeros
  DO i = 1, n
    IF (ABS(A(i,i)) < 1.0D-14) THEN
      PRINT *, "Error: Zero diagonal element at row", i
      RETURN
    END IF
  END DO

  x = 0.0D0
  converged = .FALSE.

  PRINT "(A,F5.2)", "Omega = ", omega

  DO iter = 1, max_iter
    x_old = x
    DO i = 1, n
      sum1 = 0.0D0
      DO j = 1, n
        IF (j .NE. i) sum1 = sum1 + A(i,j) * x(j)
      END DO
      x(i) = (1.0D0 - omega) * x_old(i) + omega * (b(i) - sum1) / A(i,i)
    END DO

    IF (MAXVAL(ABS(x - x_old)) < tol) THEN
      converged = .TRUE.
      EXIT
    END IF
  END DO

  IF (converged) THEN
    PRINT "(A,F10.5,A,F10.5,A,F10.5)", "x = ", x(1), ", y = ", x(2), ", z = ", x(3)
    PRINT "(A,I0)", "Iterations: ", iter
  ELSE
    PRINT *, "Did not converge within max iterations."
  END IF
  PRINT *, ""

END SUBROUTINE solve_SOR
