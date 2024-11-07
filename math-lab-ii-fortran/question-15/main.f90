!

PROGRAM FUNCTION_EVALUATION

    IMPLICIT NONE
    REAL :: X, F

    PRINT *, "Enter the value of x: "
    READ *, X
    IF(X .LT. -10.0 .OR. X .GT. 10.0)THEN
        PRINT *, "x is not in the defined range [-10, 10]"
        STOP
    END IF
    IF (X .LT. 0.0) THEN
        F = 1.0 - X**2
    ELSE IF (X .GE. 0.0 .AND. X .LT. 1.0) THEN
        F = 1.0 + 2.0*X
    ELSE IF (X .GT. 1.0) THEN
        F = 3.0 + 1.0/X
    END IF

    PRINT *, "f(x) = ", F

END PROGRAM FUNCTION_EVALUATION
