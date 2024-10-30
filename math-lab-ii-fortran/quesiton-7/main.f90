! Write a Fortran program to find first 40 terms of Fibonacci sequence, the output should have five numbers in a line.

program fibonacci_sequence
    DIMENSION N(40)
    N(1) = 1
    N(2) = 1
    DO K = 3, 40
        N(K) = N(K-1) + N(K-2)
    END DO
    PRINT*, "Here is the first 40 terms of Fibonacci Sequence:"
    PRINT 20, (N(K), K=1,40,1)
20  FORMAT(2X, 5I15)
end program fibonacci_sequence

