program inverse_3x3_matrix
    implicit none
    real :: A(3,3), invA(3,3), det
    integer :: i

    ! Define the 3x3 matrix (example)
    A = reshape([1,2,4,2,1,-2,-3,0,5],[3,3])

    ! Compute determinant (direct formula for 3x3)
    det = A(1,1) * (A(2,2)*A(3,3) - A(2,3)*A(3,2)) &
        - A(1,2) * (A(2,1)*A(3,3) - A(2,3)*A(3,1)) &
        + A(1,3) * (A(2,1)*A(3,2) - A(2,2)*A(3,1))

    ! Check if matrix is invertible (det ≠ 0)
    if (det .LE. 0) then
        print *, "Matrix is singular (determinant ≈ 0). No inverse exists!"
        stop
    endif

    ! Compute inverse using adjugate method (direct formula)
    invA(1,1) =  (A(2,2)*A(3,3) - A(2,3)*A(3,2)) / det
    invA(1,2) = -(A(1,2)*A(3,3) - A(1,3)*A(3,2)) / det
    invA(1,3) =  (A(1,2)*A(2,3) - A(1,3)*A(2,2)) / det

    invA(2,1) = -(A(2,1)*A(3,3) - A(2,3)*A(3,1)) / det
    invA(2,2) =  (A(1,1)*A(3,3) - A(1,3)*A(3,1)) / det
    invA(2,3) = -(A(1,1)*A(2,3) - A(1,3)*A(2,1)) / det

    invA(3,1) =  (A(2,1)*A(3,2) - A(2,2)*A(3,1)) / det
    invA(3,2) = -(A(1,1)*A(3,2) - A(1,2)*A(3,1)) / det
    invA(3,3) =  (A(1,1)*A(2,2) - A(1,2)*A(2,1)) / det

    ! Print original matrix
    print *, "Original Matrix A:"
    do i = 1, 3
        print '(3f10.4)', A(i,:)
    enddo

    ! Print inverse matrix
    print *, "Inverse Matrix A:"
    do i = 1, 3
        print '(3f10.4)', invA(i,:)
    enddo

end program inverse_3x3_matrix


A(1,1)  A(1,2)  A(1,3)
A(2,1)  A(2,2)  A(2,3)
A(3,1)  A(3,2)  A(3,3)
