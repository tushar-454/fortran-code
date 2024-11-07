! Variable meanings:
! x: Current approximation of the root
! xl: Lower bound of the interval
! xu: Upper bound of the interval
! xr: Midpoint of the interval
! f, fl, fu, fr: Function values at x, xl, xu, xr, respectively
! tolerance: Desired accuracy of the solution
! error: Error in the current approximation
! iteration_count: Number of iterations performed

PROGRAM ROOT_FINDING

  IMPLICIT NONE

  REAL*8 :: x, xl, xu, xr, f, fl, fu, fr, tolerance, error
  INTEGER :: iteration_count

  ! Input parameters
  xl = 0.0
  xu = 1.0
  tolerance = 1.0E-6
  iteration_count = 0

  ! Bisection method
  DO
    iteration_count = iteration_count + 1
    xr = (xl + xu) / 2.0
    fl = EXP(-xl) - xl
    fu = EXP(-xu) - xu
    fr = EXP(-xr) - xr
    IF (ABS(fr) < tolerance) THEN
      PRINT*, 'Bisection method: Root = ', xr, ' Iterations = ', iteration_count
      EXIT
    END IF
    IF (fl*fr < 0.0) THEN
      xu = xr
    ELSE
      xl = xr
    END IF
  END DO

  ! Iteration method
  x = xl
  DO
    iteration_count = iteration_count + 1
    x = EXP(-x)
    IF (ABS(EXP(-x) - x) < tolerance) THEN
      PRINT*, 'Iteration method: Root = ', x, ' Iterations = ', iteration_count
      EXIT
    END IF
  END DO

  ! Regula falsi method
  DO
    iteration_count = iteration_count + 1
    fl = EXP(-xl) - xl
    fu = EXP(-xu) - xu
    xr = xl - fl*(xu - xl)/(fu - fl)
    fr = EXP(-xr) - xr
    IF (ABS(fr) < tolerance) THEN
      PRINT*, 'Regula falsi method: Root = ', xr, ' Iterations = ', iteration_count
      EXIT
    END IF
    IF (fl*fr < 0.0) THEN
      xu = xr
    ELSE
      xl = xr
    END IF
  END DO

END PROGRAM
