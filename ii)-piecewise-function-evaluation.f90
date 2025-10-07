      PROGRAM PROBLEM2
      DOUBLE PRECISION X, RESULT
      WRITE(*,*) 'Problem 2: Piecewise Function Evaluation'
      WRITE(*,*) 'f(x) = 1 - x^2   if x < 0'
      WRITE(*,*) 'f(x) = 1 + 2x   if 0 <= x < 1'
      WRITE(*,*) 'f(x) = 3 + 1/x    if x > 1'
      WRITE(*,*) ''
      
 100  CONTINUE
      WRITE(*,*) 'Choose an option:'
      WRITE(*,*) '1. Evaluate for single x value'
      WRITE(*,*) '2. Evaluate for x in [-10,10] with step 0.5'
      WRITE(*,*) '3. Exit'
      WRITE(*,*) 'Enter your choice (1/2/3): '
      READ(*,*) ICHOICE
      
      IF (ICHOICE .EQ. 1) THEN
          CALL SINGLE_EVAL
      ELSE IF (ICHOICE .EQ. 2) THEN
          CALL RANGE_EVAL
      ELSE IF (ICHOICE .EQ. 3) THEN
          GOTO 200
      ELSE
          WRITE(*,*) 'Invalid choice! Please try again.'
      ENDIF
      
      GOTO 100
      
 200  CONTINUE
      WRITE(*,*) 'Program terminated.'
      STOP
      END

      SUBROUTINE SINGLE_EVAL
      CHARACTER*1 CHOICE
      DOUBLE PRECISION X, RESULT

 300  CONTINUE
      WRITE(*,*) 'Enter the value of x: '
      READ(*,*) X
      CALL PIECEWISE(X, RESULT)
      WRITE(*,10) X, RESULT
 10   FORMAT('f(', F20.3, ') = ', F12.10)
      
      WRITE(*,*) 'Evaluate another value? (y/n): '
      READ(*,*) CHOICE
      IF (CHOICE .EQ. 'y' .OR. CHOICE .EQ. 'Y') THEN
          GOTO 300
      ENDIF
      
      RETURN
      END

      SUBROUTINE RANGE_EVAL
      DOUBLE PRECISION X, RESULT
      WRITE(*,*) 'Evaluating f(x) for x in [-10, 10] step 0.5'
      WRITE(*,*) ''
      WRITE(*,*) '     x        f(x)'
      WRITE(*,*) '  --------   --------'
      
      X = -10.0
 400  CONTINUE
      IF (X .LE. 10.0) THEN
          CALL PIECEWISE(X, RESULT)
          WRITE(*,20) X, RESULT
 20       FORMAT(F10.1, F12.3)
          X = X + 0.5
          GOTO 400
      ENDIF
      
      WRITE(*,*) ''
      RETURN
      END

      SUBROUTINE PIECEWISE(X, RESULT)
      DOUBLE PRECISION X, RESULT
      IF (X .LT. 0.0D0) THEN
          RESULT = 1.0D0 - X**2
      ELSE IF (X .GE. 0.0D0 .AND. X .LT. 1.0D0) THEN
          RESULT = 1.0D0 + 2.0D0 * X
      ELSE IF (X .GT. 1.0D0) THEN
          RESULT = 3.0D0 + (1.0D0 / X)
      ELSE 
          PRINT *, 'Warning: f(x) is undefined for x = ', X
      ENDIF

      RETURN
      END