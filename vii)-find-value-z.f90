PROGRAM FIND_SUM_OF_Z
    IMPLICIT NONE
    REAL :: X, Y, Z, Z_VALUE, SUM_Z, INCREMENT
    REAL :: X_START, X_END, Y_START, Y_END
    INTEGER :: COUNT_X, COUNT_Y, TOTAL_COUNT
    
    ! Define the range and increment
    X_START = -3.0
    X_END = 3.0
    Y_START = -3.0
    Y_END = 3.0
    INCREMENT = 0.2
    
    ! Initialize sum and counter
    SUM_Z = 0.0
    TOTAL_COUNT = 0
    COUNT_X = 0
    
    ! Display problem statement
    PRINT *, "================================================================"
    PRINT *, "COMPUTING SUM OF Z = x² - 2xy + 3y² - 8x + 3y - 8"
    PRINT *, "================================================================"
    PRINT *, ""
    PRINT '(A,F5.1,A,F5.1)', "X range: ", X_START, " to ", X_END
    PRINT '(A,F5.1,A,F5.1)', "Y range: ", Y_START, " to ", Y_END
    PRINT '(A,F4.1)', "Increment: ", INCREMENT
    PRINT *, ""
    PRINT *, "Computing..."
    PRINT *, ""
    
    ! Nested loop to calculate sum of z for all combinations of x and y
    X = X_START
    DO WHILE (X <= X_END + 0.001)  ! Small tolerance for floating point comparison
        COUNT_Y = 0
        Y = Y_START
        DO WHILE (Y <= Y_END + 0.001)
            ! Calculate z = x² - 2xy + 3y² - 8x + 3y - 8
            Z_VALUE = X**2 - 2.0*X*Y + 3.0*Y**2 - 8.0*X + 3.0*Y - 8.0
            ! Add to sum
            SUM_Z = SUM_Z + Z_VALUE          
            ! Increment counters
            COUNT_Y = COUNT_Y + 1
            TOTAL_COUNT = TOTAL_COUNT + 1    
            ! Move to next y value
            Y = Y + INCREMENT
        END DO
      
        COUNT_X = COUNT_X + 1
        X = X + INCREMENT
    END DO
    
    ! Display results
    PRINT *, "================================================================"
    PRINT *, "RESULTS:"
    PRINT *, "================================================================"
    PRINT '(A,I5)', "Number of X values: ", COUNT_X
    PRINT '(A,I5)', "Number of Y values: ", COUNT_Y
    PRINT '(A,I6)', "Total calculations: ", TOTAL_COUNT
    PRINT *, ""
    PRINT '(A,F15.5)', "SUM OF Z = ", SUM_Z
    PRINT *, "================================================================"
    
END PROGRAM FIND_SUM_OF_Z