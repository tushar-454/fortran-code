PROGRAM LAGRANGE_INTERPOLATION
    IMPLICIT NONE
    
    ! Variable declarations
    INTEGER :: N, I, J
    REAL :: X(0:15), Y(0:15)
    REAL :: XX, SUM, L_I
    
    ! INPUT SECTION

    PRINT *, ''
    PRINT *, '========================================='
    PRINT *, '  LAGRANGE INTERPOLATION PROGRAM'
    PRINT *, '========================================='
    PRINT *, ''
    
    ! Get number of data points
    PRINT *, 'ENTER THE NUMBER OF DATA POINTS (N+1):'
    PRINT *, '  For 2-point interpolation, enter: 2'
    PRINT *, '  For 3-point interpolation, enter: 3'
    PRINT *, '  For 4-point interpolation, enter: 4'
    PRINT *, ''
    READ *, N
    N = N - 1  ! Convert to index (N intervals = N+1 points)
    
    ! Get X values
    PRINT *, ''
    PRINT *, 'ENTER', N+1, 'VALUES OF X (separated by space or Enter):'
    READ *, (X(I), I = 0, N)
    
    ! Get Y values (f(x) values)
    PRINT *, ''
    PRINT *, 'ENTER', N+1, 'VALUES OF Y (f(x)) (separated by space or Enter):'
    READ *, (Y(I), I = 0, N)
    
    ! Get the point to interpolate
    PRINT *, ''
    PRINT *, 'ENTER THE VALUE OF X WHERE YOU WANT TO FIND f(x):'
    READ *, XX
    
    SUM = 0.0
    
    ! For each data point i
    DO I = 0, N
        L_I = 1.0  ! Lagrange basis polynomial L_i(x)
        
        ! Calculate L_i(x) = Product of (x - x_j)/(x_i - x_j) for all j ≠ i
        DO J = 0, N
            IF (I .NE. J) THEN
                L_I = L_I * (XX - X(J)) / (X(I) - X(J))
            END IF
        END DO
        
        ! Add this term to the sum: L_i(x) * f(x_i)
        !PRINT *, 'Term', I+1, ': L_', I, '(x) * f(x_', I, ') = ', L_I, ' * ', Y(I), ' = ', L_I * Y(I)
        SUM = SUM + L_I * Y(I)
    END DO

    PRINT *, ''
    PRINT *, '========================================='
    PRINT *, 'RESULT:'
    PRINT *, '========================================='
    PRINT *, 'Interpolated value: f(', XX, ') = ', SUM
    PRINT *, '========================================='
    PRINT *, ''
    
END PROGRAM LAGRANGE_INTERPOLATION
