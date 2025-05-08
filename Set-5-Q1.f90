! Program to test if a matrix is symmetric or anti-symmetric, and compute C = A^2 + A, D = A^2 * A
! Matrix A is given as:
!   0   5  -4
!  -5   0   3
!   4  -3   0

program matrix_properties
    implicit none
    integer :: i, j, k
    integer :: A(3, 3), AT(3, 3), A2(3, 3), C(3, 3), D(3, 3)
    logical :: is_symmetric, is_antisymmetric

    ! Initialize matrix A
    A = reshape([0, 5, -4, &
                -5, 0, 3, &
                 4, -3, 0], [3, 3])

    ! Compute transpose of A
    do i = 1, 3
        do j = 1, 3
            AT(i, j) = A(j, i)
        end do
    end do
    ! shortened transpose calculation
    ! AT = transpose(A)

    ! Check for symmetry and anti-symmetry using .NE.
    is_symmetric = .true.
    is_antisymmetric = .true.
    do i = 1, 3
      do j = 1, 3
        if (is_symmetric .AND. (A(i, j) .NE. AT(i, j))) is_symmetric = .false.
        if (is_antisymmetric .AND. (A(i, j) .NE. -AT(i, j))) is_antisymmetric = .false.
      end do
    end do

    if (is_symmetric) then
        print *, 'Matrix A is symmetric.'
    else
        print *, 'Matrix A is NOT symmetric.'
    end if

    if (is_antisymmetric) then
        print *, 'Matrix A is anti-symmetric.'
    else
        print *, 'Matrix A is NOT anti-symmetric.'
    end if

    ! Compute A^2 = A * A
    A2 = matmul(A, A) 

    ! Compute C = A^2 + A
    C = A2 + A

    ! Compute D = A^2 * A
    D = matmul(A2, A)

    ! Print matrices C and D
    print *, 'Matrix C = A^2 + A:'
    do i = 1, 3
        print *, (C(i, j), j = 1, 3)
    end do

    print *, 'Matrix D = A^2 * A:'
    do i = 1, 3
        print *, D(i, :)
    end do

end program matrix_properties
