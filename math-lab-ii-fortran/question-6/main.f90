! Using DO Loop print the positive numbers less than 100 which are divisible by 5.

program get_positive_number
    PRINT*, "Here the all positive number less than 100 and divisible by 5:"
    N = 100
    DO I = 1, N
        IF(MOD(I,5).EQ.0) THEN
            PRINT*, I
        END IF
    END DO
end program get_positive_number

