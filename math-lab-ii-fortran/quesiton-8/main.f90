! Write a Fortran program to read in an integer N > 2 and determine if N is a prime number.

program prime_number
    PRINT*, 'Enter the value of N:'
    READ*, N
    K=2
7   IF(N.EQ.(N/K)*K) GOTO 5
    K=K+1
    IF (K.LE.N/2) GOTO 7
    PRINT*, N, 'is a prime number'
    STOP
5   PRINT*, N,'is not a prime number'
    PRINT*, K, 'is a divisor of' ,N
end program prime_number

