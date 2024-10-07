    INTEGER SUM
    READ*, N
    SUM = 0
10  SUM = SUM + MOD(N,10)
    N = N/10
    IF(N.GT.0) GOTO 10
    WRITE(6,20) SUM
20  FORMAT(2X,"SUM=",I5)
    STOP
    END
