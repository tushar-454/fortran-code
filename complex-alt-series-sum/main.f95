ISUM = 0
J=1
DO K=1,100,1
    SUM = SUM + ((K**2)*J)
    J = J * (-1)
END DO
PRINT*, "SUM=", SUM
STOP
END
