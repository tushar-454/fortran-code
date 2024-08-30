    factorial = 1
    PRINT *, "Enter a positive integer:"
    READ *, n
    DO 7 i = 1, n, 1
    factorial = factorial * i
7   CONTINUE
    PRINT *, "The factorial of", n, "is", factorial
    STOP
    END
