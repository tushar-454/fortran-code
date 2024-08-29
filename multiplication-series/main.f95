    PRINT *, "Enter the value of N [N >= 10]"
    READ *, N
    IF(N.LT.10) GOTO 30
    PROD = 1.0
    DO 7 K = 1,N,1
    PROD = PROD * REAL(2.0*K-1)/ REAL(K**2.0)
7   CONTINUE
    PRINT 20, PROD
20  FORMAT(2X,'PROD=',F10.5)
30  PRINT *, 'Error: Your input is less then 10'
    STOP
    END
