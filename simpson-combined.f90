PROGRAM SIMPSON_MENU
    IMPLICIT NONE

    INTEGER :: INTERVAL, K, CHOICE
    REAL :: LOWER_LIMIT, UPPER_LIMIT, X, FX, STEP
    REAL :: SUM, RESULT, EXACT, ERROR

    REAL :: F

    F(X) = SQRT(1.0 - X**2)

    LOWER_LIMIT = 0.0
    UPPER_LIMIT = 1.0
    INTERVAL = 24

    STEP = (UPPER_LIMIT - LOWER_LIMIT) / REAL(INTERVAL)

    PRINT*, "===================================="
    PRINT*, "SIMPSON INTEGRATION MENU"
    PRINT*, "1. SIMPSON 1/3 RULE"
    PRINT*, "2. SIMPSON 3/8 RULE"
    PRINT*, "ENTER CHOICE: "
    READ*, CHOICE

    SUM = F(LOWER_LIMIT) + F(UPPER_LIMIT)

    IF (CHOICE .EQ. 1) THEN

        DO K = 1, INTERVAL - 1
            X = LOWER_LIMIT + REAL(K) * STEP
            FX = F(X)

            IF (MOD(K,2) .EQ. 0) THEN
                SUM = SUM + 2.0 * FX
            ELSE
                SUM = SUM + 4.0 * FX
            END IF
        END DO

        RESULT = SUM * (STEP / 3.0)

        PRINT*, "USING SIMPSON 1/3 RULE"

    ELSE IF (CHOICE .EQ. 2) THEN

        DO K = 1, INTERVAL - 1
            X = LOWER_LIMIT + REAL(K) * STEP
            FX = F(X)

            IF (MOD(K,3) .EQ. 0) THEN
                SUM = SUM + 2.0 * FX
            ELSE
                SUM = SUM + 3.0 * FX
            END IF
        END DO

        RESULT = SUM * (3.0 * STEP / 8.0)

        PRINT*, "USING SIMPSON 3/8 RULE"

    ELSE
        PRINT*, "INVALID CHOICE"
        STOP
    END IF

    EXACT = ASIN(UPPER_LIMIT) - ASIN(LOWER_LIMIT)
    ERROR = ABS(EXACT - RESULT)

    PRINT*, "RESULT =", RESULT
    PRINT*, "EXACT  =", EXACT
    PRINT*, "ERROR  =", ERROR

END PROGRAM SIMPSON_MENU
