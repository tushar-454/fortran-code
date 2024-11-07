! write a Fortran program to find sum of the series:
! 1.(1+1/2).(1+1/2+1/3).....(1+1/2+1/3+...+1/n))

program series_sum
    PROD = 1.0
    PRINT*, "Enter the value of n:"
    READ*, N
    DO J = 1,N
        SUM = 0.0
        DO K = 1,J
            SUM = SUM + 1.0/REAL(K)
        END DO
        PROD = PROD * SUM
    END DO
    PRINT 20, PROD
20  FORMAT (3X,"Sum of the series:", F12.5)
end program series_sum
