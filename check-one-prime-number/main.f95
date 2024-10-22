    PRINT*, 'ENTER THE VALUE OF N'
    READ* , N
    K=2
7   IF(N.EQ.(N/K) *K) GOTO 5
    K=K+1
    IF(K.LE.N/2) GOTO 7
    PRINT*, N, 'IS A PRIME NO. '
    STOP
5   PRINT*, N,'IS NOT A PRIME NO.'
    PRINT*, K, 'IS A DIVISOR OF' ,N
    END
