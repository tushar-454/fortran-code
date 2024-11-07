! Write a Fortran program to find positive root of the equation cos(x)-x*e^x =0 using Newton-Raphson method.

PROGRAM NEWTON_RAPHSON

  IMPLICIT NONE

  REAL*8 :: x, tolerance, root
  INTEGER :: max_iterations

  ! Parameters
  x = 0.5d0
  tolerance = 1.0d-6
  max_iterations = 100

  PRINT *, "Finding root of f(x) = cos(x) - x * exp(x) = 0 using Newton-Raphson method"

  CALL NEWTON_RAPHSON_SUBROUTINE(x, tolerance, max_iterations, root)

  PRINT *, "Root (Newton-Raphson Method): ", root

CONTAINS

  ! Function defining f(x) = cos(x) - x * exp(x)
  REAL*8 FUNCTION f(x)
    REAL*8, INTENT(IN) :: x
    f = COS(x) - x * EXP(x)
  END FUNCTION f

  ! Derivative of f(x), f'(x) = -sin(x) - e^x - x * e^x
  REAL*8 FUNCTION df(x)
    REAL*8, INTENT(IN) :: x
    df = -SIN(x) - EXP(x) * (1.0d0 + x)
  END FUNCTION df

  ! Newton-Raphson Method
  SUBROUTINE NEWTON_RAPHSON_SUBROUTINE(x0, tolerance, max_iterations, root)
    REAL*8, INTENT(IN) :: x0, tolerance
    INTEGER, INTENT(IN) :: max_iterations
    REAL*8, INTENT(OUT) :: root
    REAL*8 :: fx, dfx
    INTEGER :: ITER

    x = x0
    DO ITER = 1, max_iterations
      fx = f(x)
      dfx = df(x)

      IF (ABS(dfx) < 1.0d-10) THEN
        PRINT *, "Newton-Raphson method: Derivative near zero; stopping iteration"
        root = x
        RETURN
      END IF

      x = x - fx / dfx

      IF (ABS(fx) < tolerance) THEN
        root = x
        RETURN
      END IF
    END DO

    root = x
    PRINT *, "Newton-Raphson method: Max iterations reached without convergence"
  END SUBROUTINE NEWTON_RAPHSON_SUBROUTINE

END PROGRAM NEWTON_RAPHSON
