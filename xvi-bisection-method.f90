PROGRAM ROOT_FINDING
  IMPLICIT NONE
  
  ! Variable declarations with descriptive names
  REAL(8) :: x_lower, x_upper, x_root
  REAL(8) :: f_lower, f_upper, f_root
  REAL(8) :: tolerance
  INTEGER :: iter_bisection
  INTEGER :: max_iterations
  
  ! Initialize parameters
  x_lower = 0.0D0
  x_upper = 1.0D0
  tolerance = 1.0D-6
  max_iterations = 1000
  
  PRINT*, REPEAT('=', 60)
  PRINT*, 'SOLVING EQUATION: e^(-x) - x = 0'
  PRINT*, 'INITIAL INTERVAL: [', x_lower, ',', x_upper, ']'
  PRINT*, 'TOLERANCE:', tolerance
  PRINT*, REPEAT('=', 60)
  PRINT*

  PRINT*, '--- BISECTION METHOD ---'
  iter_bisection = 0
  
  DO WHILE (iter_bisection < max_iterations)
    iter_bisection = iter_bisection + 1
    
    ! Calculate midpoint
    x_root = (x_lower + x_upper) / 2.0D0
    
    ! Evaluate function at endpoints and midpoint
    f_lower = EXP(-x_lower) - x_lower
    f_upper = EXP(-x_upper) - x_upper
    f_root = EXP(-x_root) - x_root
    
    ! Check convergence
    IF (ABS(f_root) < tolerance) THEN
      PRINT*, 'Root found: ', x_root
      PRINT*, 'Iterations: ', iter_bisection
      PRINT*, 'Function value: ', f_root
      EXIT
    END IF
    
    ! Update interval
    IF (f_lower * f_root < 0.0D0) THEN
      x_upper = x_root
    ELSE
      x_lower = x_root
    END IF
  END DO
  
END PROGRAM ROOT_FINDING