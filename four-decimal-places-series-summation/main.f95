    READ *, X,K
    SUM = 0.0
    DO 7 J=0,K,1
    SUM = SUM + X ** J
7   CONTINUE
    PRINT 20, SUM
20  FORMAT(2X,'SUM=',F10.4)
    STOP
    END
