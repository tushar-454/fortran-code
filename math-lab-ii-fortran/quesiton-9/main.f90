! Write a Fortran program which prints all the prime numbers less than 500

program prime_numbers
    PRINT*, 'Enter the value of N:'
    READ*, N

    DO I = 2, N
        isPrime = 1

        DO J = 2, I/2
            IF (MOD(I, J).EQ.0) THEN
                isPrime = 0
                EXIT
            END IF
        END DO

        IF (isPrime.EQ.1) THEN
            PRINT*, I, 'is a prime number'
        END IF
    END DO
end program prime_numbers

