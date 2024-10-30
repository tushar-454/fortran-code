! Write a program which reads a 3 x 2 matrix column wise and prints the elements row wise.

program read_column_write_row
    PARAMETER(m = 3, n = 2)
    DIMENSION matrix(m, n)

    PRINT*, "Enter the elements of the matrix (3x2) column-wise:"
    DO j = 1, n
        DO i = 1, m
            PRINT*, "Enter element (", i, ",", j, "):"
            READ*, matrix(i, j)
        END DO
    END DO
    PRINT*, "Matrix elements row-wise:"
    DO i = 1, m
        PRINT*, (matrix(i, j), j = 1, n)
    END DO

end program read_column_write_row
