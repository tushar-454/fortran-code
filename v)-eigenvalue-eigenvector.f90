program dominant_eigenvalue
    implicit none
    ! Variable declarations
    real(8) :: A(3,3)                    ! The matrix
    real(8) :: V(3), V1(3)               ! Current and next eigenvector
    real(8) :: eigenvalue, prev_eigenvalue
    real(8) :: tolerance, error
    integer :: i, max_iter, iter
    integer :: max_index
    real(8) :: max_val
    
    ! Initialize parameters
    tolerance = 1.0d-6
    max_iter = 100
    
    ! Define the matrix A
    A(1,1) = 1.0d0;  A(1,2) = 6.0d0;  A(1,3) = 1.0d0
    A(2,1) = 1.0d0;  A(2,2) = 2.0d0;  A(2,3) = 0.0d0
    A(3,1) = 0.0d0;  A(3,2) = 0.0d0;  A(3,3) = 3.0d0
    
    ! Initial eigenvector X^(0)
    V(1) = 1.0d0
    V(2) = 0.0d0
    V(3) = 0.0d0
    
    ! Display initial data
    print *, "Matrix A:"
    do i = 1, 3
        print '(3F10.4)', A(i,:)
    end do
    print *, ""
    
    print *, "Initial vector X^(0):"
    do i = 1, 3
        print '(F10.4)', V(i)
    end do
    print *, ""
    print *, "Starting Power Iteration Method..."
    print *, ""
    
    prev_eigenvalue = 0.0d0
    
    ! Power iteration loop
    do iter = 1, max_iter
        ! Step 1: Multiply A * V
        V1 = MATMUL(A, V)
        
        ! Step 2: Extract eigenvalue using first non-zero component
        ! Use the first component to maintain sign consistency
        if (abs(V(1)) > 1.0d-10) then
            eigenvalue = V1(1) / V(1)
        else if (abs(V(2)) > 1.0d-10) then
            eigenvalue = V1(2) / V(2)
        else
            eigenvalue = V1(3) / V(3)
        end if
        
        ! Step 3: Normalize by the component with largest absolute value
        ! This keeps the eigenvector normalized
        max_val = abs(V1(1))
        max_index = 1
        do i = 2, 3
            if (abs(V1(i)) > max_val) then
                max_val = abs(V1(i))
                max_index = i
            end if
        end do
        V1 = V1 / V1(max_index)
        
        ! Check for convergence
        error = abs(eigenvalue - prev_eigenvalue)
        
        ! Display iteration information
        print '(A,I3,A,F12.8,A,E12.4)', "Iteration ", iter, &
              ": Eigenvalue = ", eigenvalue, ", Error = ", error
        print *, "   Eigenvector: "
        do i = 1, 3
            print '(A,F10.6)', "      ", V1(i)
        end do
        
        if (error < tolerance .and. iter > 1) then
            print *, ""
            print *, "Convergence achieved!"
            exit
        end if
        
        ! Update for next iteration
        V = V1
        prev_eigenvalue = eigenvalue
    end do
    
    ! Display final results
    print *, ""
    print *, "================================="
    print *, "FINAL RESULTS:"
    print *, "================================="
    print '(A,F12.8)', "Dominant Eigenvalue = ", eigenvalue
    print *, "Corresponding Eigenvector:"
    do i = 1, 3
        print '(F12.8)', V1(i)
    end do
    print *, ""
    print '(A,I3,A)', "Converged in ", iter, " iterations"
    
end program dominant_eigenvalue