program matrix_properties
    implicit none
    integer :: i = 3
    integer :: A(3, 3)
    integer :: C(3, 3), D(3, 3)
    logical :: is_symmetric, is_antisymmetric

    ! Define the matrix A
    A = reshape([0, 5, -10, &
                -5, 0, 6, &
                10, -6, 0], [3, 3])

    ! Check for symmetry
    is_symmetric = all(A == transpose(A))
    is_antisymmetric = all(A == -transpose(A))

    ! Compute C = A^2 + A
    C = matmul(A, A) + A

    ! Compute D = A^2 * A
    D = matmul(matmul(A, A), A)

    ! Print results
    print *, 'Matrix A:'
    do i = 1, 3
        print *, A(i, :)
    end do
    
    if (is_symmetric)  print *, 'A is symmetric.'
    if (.not. is_symmetric)  print *, 'A is not symmetric.'
    if (is_antisymmetric)  print *, 'A is antisymmetric.'
    if (.not. is_antisymmetric)  print *, 'A is not antisymmetric.'

    print *, 'Matrix C = A^2 + A:'
    do i = 1, 3
        print *, C(i, :)
    end do
    print *, 'Matrix D = A^2 * A:'
    do i = 1, 3
        print *, D(i, :)
    end do
end program matrix_properties
