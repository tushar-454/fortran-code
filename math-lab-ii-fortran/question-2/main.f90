! write a Fortran program to find sum of the series:
! 1+(1+1/2)+(1+1/2+1/3)+...+(1+1/2+1/3+...+1/n))

program series_sum
    SUM = 0.0
    PRINT*, "Enter the value of n:"
    READ*, N
    DO J = 1,N
        DO K = 1,J
            SUM = SUM + 1.0/REAL(K)
        END DO
    END DO
    PRINT 20, SUM
20  FORMAT (3X,"Sum of the series:", F12.5)
end program series_sum
