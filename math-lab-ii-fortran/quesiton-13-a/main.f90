! Find the trace of A

program find_trace
    PARAMETER(m = 3)
    DIMENSION matrix(m, m)

    PRINT*, "Enter the elements of the matrix (3x3) row-wise:"
    DO i = 1, m
        DO j = 1, m
            PRINT*, "Enter element (", i, ",", j, "):"
            READ*, matrix(i, j)
        END DO
    END DO
    trace = matrix(1,1) + matrix(2,2) + matrix(3,3)
    PRINT*, "The trace of matrix A is:", trace

end program find_trace
