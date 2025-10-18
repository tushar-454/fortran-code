PROGRAM SIMPSONS_38_TRAPEZOIDAL
    IMPLICIT NONE
    INTEGER :: INTERVALS, K
    REAL :: LOWER_LIMIT, UPPER_LIMIT, X1, F1, TRAP_SUM, SIM38_SUM
    REAL :: TRAPEZOIDAL_RESULT, SIMPSONS_38_RESULT, EXACT, ERR_TRAP, ERR_SIM38, STEP
    REAL :: PI, X, F

    ! Define the function f(x) = e^(sin(x))
    F(X) = EXP(SIN(X))
    
    ! Define PI
    PI = 4.0 * ATAN(1.0)
    
    ! Integration limits: from 0 to π/2
    LOWER_LIMIT = 0.0
    UPPER_LIMIT = PI / 2.0
    
    ! Number of intervals (must be multiple of 3 for Simpson's 3/8)
    INTERVALS = 24
    STEP = (UPPER_LIMIT - LOWER_LIMIT) / FLOAT(INTERVALS)
    
    ! Initialize sums for Trapezoidal Rule
    TRAP_SUM = (F(LOWER_LIMIT) + F(UPPER_LIMIT))
    
    ! Initialize sums for Simpson's 3/8 Rule
    SIM38_SUM = (F(LOWER_LIMIT) + F(UPPER_LIMIT))
    
    ! Calculate the intermediate points
    DO K = 1, INTERVALS - 1
        X1 = LOWER_LIMIT + FLOAT(K) * STEP
        F1 = F(X1)
        
        ! For Trapezoidal Rule: add 2*f(x) for all intermediate points
        TRAP_SUM = TRAP_SUM + 2.0 * F1
        
        ! For Simpson's 3/8 Rule:
        ! If k is multiple of 3, coefficient is 2
        ! Otherwise, coefficient is 3
        IF (MOD(K, 3) .EQ. 0) THEN
            SIM38_SUM = SIM38_SUM + 2.0 * F1
        ELSE
            SIM38_SUM = SIM38_SUM + 3.0 * F1
        ENDIF
    ENDDO
    
    ! Calculate final results
    TRAPEZOIDAL_RESULT = TRAP_SUM * (STEP / 2.0)
    SIMPSONS_38_RESULT = SIM38_SUM * (3.0 * STEP / 8.0)
    
    ! Calculate exact value using more intervals (numerical approximation)
    ! Using very fine intervals as "exact" value
    EXACT = 0.0
    DO K = 0, 10000
        X1 = LOWER_LIMIT + FLOAT(K) * (UPPER_LIMIT - LOWER_LIMIT) / 10000.0
        IF (K == 0 .OR. K == 10000) THEN
            EXACT = EXACT + F(X1)
        ELSE IF (MOD(K, 2) == 0) THEN
            EXACT = EXACT + 2.0 * F(X1)
        ELSE
            EXACT = EXACT + 4.0 * F(X1)
        ENDIF
    ENDDO
    EXACT = EXACT * ((UPPER_LIMIT - LOWER_LIMIT) / 10000.0) / 3.0
    
    ! Calculate errors
    ERR_TRAP = ABS(EXACT - TRAPEZOIDAL_RESULT)
    ERR_SIM38 = ABS(EXACT - SIMPSONS_38_RESULT)
    
    ! Display results
    PRINT *, "=========================================================="
    PRINT *, "INTEGRATION: e^(sin(x)) FROM 0 TO π/2"
    PRINT *, "=========================================================="
    PRINT *, ""
    PRINT *, "Integration Limits:"
    PRINT '(A,F10.6)', "  Lower Limit (0)   = ", LOWER_LIMIT
    PRINT '(A,F10.6)', "  Upper Limit (π/2) = ", UPPER_LIMIT
    PRINT '(A,I5)', "  Number of Intervals = ", INTERVALS
    PRINT *, ""
    PRINT *, "Results:"
    PRINT *, "----------------------------------------------------------"
    PRINT '(A,F12.8)', "  Exact Value (Approximation) = ", EXACT
    PRINT *, ""
    PRINT '(A,F12.8)', "  Trapezoidal Rule Result     = ", TRAPEZOIDAL_RESULT
    PRINT '(A,E12.5)', "  Error in Trapezoidal        = ", ERR_TRAP
    PRINT *, ""
    PRINT '(A,F12.8)', "  Simpson's 3/8 Rule Result   = ", SIMPSONS_38_RESULT
    PRINT '(A,E12.5)', "  Error in Simpson's 3/8      = ", ERR_SIM38
    PRINT *, ""
    PRINT *, "=========================================================="
    PRINT *, "Comparison:"
    PRINT *, "----------------------------------------------------------"
    IF (ERR_SIM38 .LT. ERR_TRAP) THEN
        PRINT *, "  Simpson's 3/8 rule is MORE ACCURATE"
        PRINT '(A,F10.2,A)', "  (Error is ", (ERR_TRAP/ERR_SIM38), " times smaller)"
    ELSE
        PRINT *, "  Trapezoidal rule is MORE ACCURATE"
        PRINT '(A,F10.2,A)', "  (Error is ", (ERR_SIM38/ERR_TRAP), " times smaller)"
    ENDIF
    PRINT *, "=========================================================="
    
END PROGRAM SIMPSONS_38_TRAPEZOIDAL
