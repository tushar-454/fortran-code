! Write a Fortran Program which reads a positive integer N ≥15 and calculates to
! five decimal places the product- (1/1^2).(3/2^2).(5/3^2)...(2N-1)/N^2

program calculate_product
    PRINT*, "Enter the value of N:"
    READ*, N
    IF(N.GT.15) THEN
        PRINT*, "The value of N can't more then 15"
        STOP
    END IF
    IF(N.LT.0) THEN
        PRINT*, "The value of N can't less then 0"
        STOP
    END IF
    PROD = 1.0
    DO I = 2, N
        PROD = PROD * (REAL(2*I-1)/REAL(I**2))
    END DO

    PRINT 20, PROD
20  FORMAT (3X,"Calculated value of Product to five decimal places:", F12.5)
end program calculate_product

