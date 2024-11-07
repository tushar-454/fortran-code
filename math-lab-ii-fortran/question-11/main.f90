! You have two matrices A of order 2 x 3 and B of order 3 x 3. Write a Fortran program to compute the matrix C = AB

program matrix_multiplication
    PARAMETER(mA = 2,nA = 2,nB = 2)
    DIMENSION A(mA, nA), B(nA, nB), C(mA, nB)

    PRINT*, "Enter the values for matrix A (", mA, "x", nA, "):"

    DO i = 1, mA
        PRINT*, "Enter the value of row ", i, ":"
        READ*, (A(i, j), j = 1, nA)
    END DO
    PRINT*, "Enter the values for matrix B (", nA, "x", nB, "):"
    DO i = 1, nA
        PRINT*, "Enter the value of row ", i, ":"
        READ*, (B(i, j), j = 1, nB)
    END DO

    C = 0.0

    DO i = 1, mA
        DO j = 1, nB
            DO k = 1, nA
                C(i, j) = C(i, j) + A(i, k) * B(k, j)
            END DO
        END DO
    END DO

    PRINT*, 'Matrix A:'
    DO i = 1, mA
        PRINT*, (A(i, j), j = 1, nA)
    END DO

    PRINT*, 'Matrix B:'
    DO i = 1, nA
        PRINT*, (B(i, j), j = 1, nB)
    END DO

    PRINT*, 'Matrix C (Result of A * B):'
    DO i = 1, mA
        PRINT*, (C(i, j), j = 1, nB)
    END DO

end program matrix_multiplication


