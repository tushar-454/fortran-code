! Write a Fortran program. Which will determine the value of x up to six decimal places,
! using the series Pi^2/6=1+(1/2^2)+(1/3^2)+(1/4^2)+...1000 terms

program calculate_pi
    N = 1000
    SUM = 1.0

    DO I = 2, N
        SUM = SUM + 1.0 / (I**2)
    END DO

    PI = SQRT(6.0 * SUM)
    print 20, PI
20  FORMAT (3X,"Calculated value of pi to six decimal places:",F12.6)
end program calculate_pi

