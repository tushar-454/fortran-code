! Q3. Test whether or not the given matrix A is
! (a) Idempotent Matrix [A^2 = A]
! (b) Orthogonal Matrix [A^T A = I]
! (c) Involutory Matrix [A^2 = I]
! Input: A = | 2 -2 -4 |
!            |-1  3  4 |
!            | 1 -2 -3 |

program matrix_problem
    implicit none
    integer :: i, j
    real :: A(3,3), IDEN(3,3), AA(3,3), AT(3,3), ATA(3,3)
    logical :: idempotent, orthogonal, involutory

    ! Prompt user to enter the matrix A row-wise
    print*, "Enter the matrix A: (row wise)"
    read*, ((A(i,j), j=1,3), i=1,3)

    ! Display the entered matrix
    print*, "Matrix A is:"
    do i = 1, 3
        print*, A(i, :)
    end do

    ! Calculate A^2 (A multiplied by itself)
    AA = matmul(A, A)

    ! Check if A is Idempotent (A^2 = A)
    idempotent = .true.
    do i = 1, 3
        do j = 1, 3
            if (A(i,j) .NE. AA(i,j)) then
                idempotent = .false.
                exit
            end if
        end do
        if (.not. idempotent) exit
    end do

    if (idempotent) then
        print*, "Matrix A is Idempotent"
    else
        print*, "Matrix A is not Idempotent"
    end if

    ! Create 3x3 Identity matrix
    IDEN = reshape([1.0, 0.0, 0.0, &
                 0.0, 1.0, 0.0, &
                 0.0, 0.0, 1.0], shape=[3,3])

    ! Calculate A^T (transpose of A) and A^T * A
    AT = transpose(A)
    ATA = matmul(AT, A)

    ! Check if A is Orthogonal (A^T * A = I)
    orthogonal = .true.
    do i = 1, 3
        do j = 1, 3
            if (ATA(i,j) .NE. IDEN(i,j)) then
                orthogonal = .false.
                exit
            end if
        end do
        if (.not. orthogonal) exit
    end do

    if (orthogonal) then
        print*, "Matrix A is Orthogonal"
    else
        print*, "Matrix A is not Orthogonal"
    end if

    ! Check if A is Involutory (A^2 = I)
    involutory = .true.
    do i = 1, 3
        do j = 1, 3
            if (AA(i,j) .NE. IDEN(i,j)) then
                involutory = .false.
                exit
            end if
        end do
        if (.not. involutory) exit
    end do

    if (involutory) then
        print*, "Matrix A is Involutory"
    else
        print*, "Matrix A is not Involutory"
    end if

end program matrix_problem
