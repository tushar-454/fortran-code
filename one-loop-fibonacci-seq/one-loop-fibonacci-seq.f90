DIMENSION N(50)
N(1) = 1
N(2) = 1
DO K = 3,50
    N(K) = N(K-1)+N(K-2)
ENDDO
PRINT 10, (N(K), K=1,50)
10 FORMAT(2X, 6I12)
STOP
END
