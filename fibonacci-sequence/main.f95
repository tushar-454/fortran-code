    DIMENSION N(40)
    N(1) = 1
    N(2) = 1
    DO 7 K = 3,40,1
    N(K) = N(K-1) + N(K-2)
7   CONTINUE
    PRINT 20, (N(K), K=1,40,1)
20  FORMAT(2X, 5I15)
    STOP
    END
