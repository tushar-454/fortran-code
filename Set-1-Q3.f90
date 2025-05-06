program matrix_problem
    integer :: i, j, A(3,3), AT(3,3), C(3,3), D(3,3), AAT(3,3), AA(3,3)
    logical :: symmetric, antisymmetric, orthogonal
    ! read matrix
    print*, "Enter matrix value row wise:"
    read*, ((A(i, j), j = 1, 3), i = 1, 3)
    !preview matrix A
    print*, "Matrix A is:"
    do i = 1, 3
        print*, A(i,:)
    end do
    !make transpose matrix of A
    do i = 1, 3
        do j = 1, 3
            AT(i, j) = A(j, i)
        end do
    end do
    !preview matrix AT
    print*, "Matrix AT is:"
    do i = 1, 3
        print*, AT(i,:)
    end do
    !check A = AT START
    symmetric = .true.
    do i = 1, 3
        do j = 1, 3
            if (AT(i, j) .NE. A(i, j)) then
                symmetric = .false.
                exit
            end if
        end do
        if (.not. symmetric) exit
    end do

    if(.not. symmetric) then
        print*, "A is not Symmetric"
    else
        print*, "A is Symmetric"
    end if
    !check A = AT STOP

    !check A = -AT START
    antisymmetric = .true.
    do i = 1, 3
        do j = 1, 3
            if (-AT(i, j) .NE. A(i, j)) then
                antisymmetric = .false.
                exit
            end if
        end do
        if (.not. antisymmetric) exit
    end do

    if(.not. antisymmetric) then
        print*, "A is not antisymmetric"
    else
        print*, "A is antisymmetric"
    end if
    !check A = -AT STOP

    !check A is Orthogonal START
    orthogonal = .true.
    ! A x AT - START
    do i = 1, 3
        do j = 1, 3
            AAT(i, j) = 0
            do k = 1, 3
                AAT(i, j) = AAT(i, j) + A(i, k) * AT(k, j)
            end do
        end do
    end do
    ! A x AT - STOP
    !Altranative for A x AT use AAT = matmul(A, AT)
    !preview of matrix AAT
    print*, "Matrix of AAT:"
    do i = 1, 3
        print*, AAT(i, :)
    end do
    ! check orthogonal logic START
    do i = 1, 3
        do j = 1, 3
            if(i.EQ.j) then
                if(AAT(i, j).NE. 1) then
                    orthogonal = .false.
                    exit
                end if
            else
                if(AAT(i, j).NE. 0) then
                    orthogonal = .false.
                    exit
                end if
            end if
        end do
        if (.not. orthogonal) exit
    end do
    ! check orthogonal logic STOP
    if (.not. orthogonal) then
        print*, "A is not Orthogonal"
    else
        print*, "A is Orthogonal"
    end if
    !check A is Orthogonal STOP

    AA = matmul(A, A)

    !calculate C - START
    C = AA + A
    print*, "Matrix C is:"
    do i = 1, 3
        print*, C(i, :)
    end do
    !calculate C - STOP

    !calculate D - START
    D = matmul(AA , A)
    print*, "Matrix D is:"
    do i = 1, 3
        print*, D(i, :)
    end do
    !calculate D - STOP

end program
