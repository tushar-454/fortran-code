PROGRAM BISECTION
    F(X)=EXP(-X)-X
    TOL=0.0001
    PRINT*, "Enter XO and XN"
10  READ*, XO,XN

20  IF(F(XO)*F(XN).LT.0) THEN
        C=(XO+XN)/2.0
    ELSE
        PRINT*, "Change for another XO & XN"
        GOTO 10
    END IF

    IF(F(XO)*F(C).LT.0) THEN
        XN = C
    ELSE
        XO = C
    END IF

    IF(ABS(XN-XO).GT.TOL) GOTO 20
    PRINT*, "The root is=", C
END PROGRAM BISECTION
