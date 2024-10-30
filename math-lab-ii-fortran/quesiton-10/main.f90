! Write a Fortran Program to compute the roots of the quadratic equation Ax^2 + Bx + C = 0 where A is not zero.

program roots_of_the_quadratic_equation
    character(len=20) :: formatted_P
    character(len=20) :: formatted_Q

    PRINT *, "Enter the coefficient value of a:"
    READ *, A
    PRINT *, "Enter the coefficient value of b:"
    READ *, B
    PRINT *, "Enter the coefficient value of c:"
    READ *, C

    IF(A.EQ.0.0) THEN
        PRINT*, "Value of A can't be a 0. Try again."
        STOP
    END IF

    D = B**2 - 4.0 * A * C

    IF(D.GE.0.0) THEN
        root1 = (-B+SQRT(D))/(2.0*A)
        root2 = (-B-SQRT(D))/(2.0*A)
        PRINT *, "The roots are real and different"
        PRINT 10, root1
        PRINT 20, root2
    10  FORMAT (3X,"Root 1:",F10.2)
    20  FORMAT (3X,"Root 2:",F10.2)
    ELSE
        P = -B/(2.0*A)
        Q = SQRT(ABS(D))/(2.0*A)
        WRITE(formatted_P, '(F10.2)') P
        WRITE(formatted_Q, '(F10.2)') Q
        PRINT*,"Root 1:",formatted_P,"+I",formatted_Q
        PRINT*,"Root 2:",formatted_P,"-I",formatted_Q
    END IF
end program roots_of_the_quadratic_equation

