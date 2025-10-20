PROGRAM FIXED_POINT_ITERATION
  IMPLICIT NONE
  ! Variable declarations with descriptive names
  REAL(8) :: x_current, x_next
  REAL(8) :: tolerance, error
  INTEGER :: iter_count
  INTEGER :: max_iterations
  
  ! Initialize parameters
  x_current = 0.5D0          ! Initial guess
  tolerance = 0.00005D0      ! For 4 decimal places accuracy
  max_iterations = 1000
  iter_count = 0
  
  PRINT*, REPEAT('=', 60)
  PRINT*, 'SOLVING EQUATION: cos(x) = 3x - 1'
  PRINT*, 'USING FIXED POINT ITERATION METHOD'
  PRINT*, 'REARRANGED FORM: x = (cos(x) + 1) / 3'
  PRINT*, 'INITIAL GUESS: ', x_current
  PRINT*, 'TOLERANCE (4 decimal places):', tolerance
  PRINT*, REPEAT('=', 60)
  PRINT*
  
  PRINT*, 'ITERATION', '      x_current', '       x_next', '          ERROR'
  PRINT*, REPEAT('-', 60)
  ! ===========================
  ! FIXED POINT ITERATION METHOD
  ! ===========================
  DO WHILE (iter_count < max_iterations)
    iter_count = iter_count + 1
    
    ! Fixed point iteration: x = (cos(x) + 1) / 3
    x_next = (COS(x_current) + 1.0D0) / 3.0D0
    
    ! Calculate error
    error = ABS(x_next - x_current)
    
    ! Display iteration details
    PRINT '(I7, 3F16.5)', iter_count, x_current, x_next, error
    
    ! Check convergence
    IF (error < tolerance) THEN
      PRINT*, REPEAT('-', 60)
      PRINT*, 'Root found (4 decimal places): ', x_next
      PRINT*, 'Total iterations: ', iter_count
      PRINT*, 'Final error: ', error
      PRINT*
      
      ! Verify the solution
      PRINT*, 'VERIFICATION:'
      PRINT*, 'cos(x) = ', COS(x_next)
      PRINT*, '3x - 1 = ', 3.0D0 * x_next - 1.0D0
      PRINT*, REPEAT('=', 60)
      EXIT
    END IF
    
    ! Update x_current for next iteration
    x_current = x_next
    
    ! Check if maximum iterations reached
    IF (iter_count >= max_iterations) THEN
      PRINT*
      PRINT*, 'WARNING: Maximum iterations reached without convergence!'
      PRINT*, 'Last approximation: ', x_next
      EXIT
    END IF
  END DO
  
END PROGRAM FIXED_POINT_ITERATION
