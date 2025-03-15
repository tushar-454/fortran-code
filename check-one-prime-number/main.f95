    PRINT*, 'ENTER THE VALUE OF N'
    READ* , N
    IF(N.EQ.1 .OR. N.EQ.2) GOTO 20
    K=2
7   IF(N.EQ.(N/K) *K) GOTO 5
    K=K+1
    IF(K.LE.N/2) GOTO 7
20  PRINT 30, N
30  FORMAT(3X,I10, ' is a prime number')
    STOP
5   PRINT*, N,'is not a prime number'
    PRINT*, K, 'is a divisor of' ,N
    END
