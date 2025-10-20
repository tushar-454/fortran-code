PROGRAM EVALFN
      CHARACTER*1 CHOICE
      DOUBLE PRECISION X, RESULT
      
 100  CONTINUE
      WRITE(*,*) 'Enter the value of x: '
      READ(*,*) X
      CALL PIECEWISE(X, RESULT)
      WRITE(*,10) X, RESULT
 10   FORMAT('f(', F20.3, ') = ', F20.3)
      WRITE(*,*) 'Do you want to evaluate another value? (y/n): '
      read(*,*) CHOICE
      IF (CHOICE .EQ. 'n' .OR. CHOICE .EQ. 'N') THEN
          GOTO 200
      ENDIF
      GOTO 100
 200  CONTINUE
      WRITE(*,*) 'Program terminated.'
      STOP
      END

      SUBROUTINE PIECEWISE(X, RESULT)
      DOUBLE PRECISION X, RESULT
      IF (X .EQ. 0.0D0 .OR. X .EQ. -0.0D0) THEN
          RESULT = 0.0D0
      ELSE IF (X .LE. 0.0D0) THEN
          RESULT = -1.0D0 * X
      ELSE IF (X .GT. 0.0D0 .AND. X .LT. 1.0D0) THEN
          RESULT = X
      ELSE
          RESULT = 1.0D0 - X
      ENDIF
      RETURN
      END