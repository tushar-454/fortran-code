! write a Fortran program to find sum of the series:
! 1^2-2^2+3^2-4^2+....+(-1)^nN^2.

program series_sum
    SUM = 0
    I = -1
    K = 1
    PRINT*, "Enter the last number of series:"
    READ*, N
    DO K = 1, N
        SUM = SUM + (-1*I)*K**2
        I = I * -1
    END DO
    PRINT*, "Sum of the series:", SUM
end program series_sum
