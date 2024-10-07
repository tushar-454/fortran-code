PRINT *, "Enter the coefficient value of a:"
READ *, A
PRINT *, "Enter the coefficient value of b:"
READ *, B
PRINT *, "Enter the coefficient value of c:"
READ *, C
D = B**2 - 4.0 * A * C
IF(D.GE.0.0) THEN
    root1 = (-B+SQRT(D))/(2.0*A)
    root2 = (-B-SQRT(D))/(2.0*A)
    PRINT *, "The roots are real and different"
    PRINT *, "Root 1 =", root1
    PRINT *, "Root 2 =", root2
ELSE
    P = -B/(2.0*A)
    Q = SQRT(ABS(D))/(2.0*A)
    PRINT*,"1st Root=",P,"+I",Q
    PRINT*,"2nd Root=",P,"-I",Q
END IF
STOP
END
