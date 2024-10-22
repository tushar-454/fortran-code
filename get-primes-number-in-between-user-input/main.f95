PRINT*, 'Enter the value of N:'
READ*, N

DO i = 2, N
    isPrime = 1

    DO j = 2, i/2
        IF (MOD(i, j) == 0) THEN
            isPrime = 0
            EXIT
        END IF
    END DO

    IF (isPrime == 1) THEN
        PRINT*, i, 'is a prime number'
    END IF
END DO
STOP
END
