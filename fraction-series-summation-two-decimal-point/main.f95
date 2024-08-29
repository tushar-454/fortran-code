    PRINT *, "Enter your limit number"
    READ *, N
    SUM = 0.0
    DO 7 K = 1, N, 1
    SUM = SUM + REAL(K) / REAL(K+1)
7   CONTINUE
    PRINT 10, SUM
10  FORMAT(2X,'SUM=', F10.2)
    STOP
    END
