    REAL NUMBER
    K = 1
    SUM = 0.0
7   READ*, NUMBER
    SUM = SUM + NUMBER
    K = K + 1
    IF(K.LE.3) GOTO 7
    AVERAGE = SUM/3.0
    PRINT*, SUM, AVERAGE
    STOP
    END
