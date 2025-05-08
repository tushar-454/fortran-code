! Gauss-Jordan elimination to solve a 3x3 system of linear equations
! System:
! x1 + 3x2 + 8x3 =  4
! x1 + 4x2 + 3x3 = -2
! x1 + 3x2 + 4x3 =  1

program gauss_jordan
    implicit none
    integer :: i, j
    real :: a(3,4), factor, temp
    real :: x(3)

    ! Initialize the augmented matrix [A|b]
    a = reshape([ &
        1.0, 3.0, 8.0,  4.0, &
        1.0, 4.0, 3.0, -2.0, &
        1.0, 3.0, 4.0,  1.0  &
    ], [3,4])

    ! Gauss-Jordan elimination for 3x3 system
    do i = 1, 3
        ! Make the diagonal element 1
        temp = a(i,i)
        a(i,:) = a(i,:) / temp

        ! Make other elements in column i zero
        do j = 1, 3
            if (j .NE. i) then
                factor = a(j,i)
                a(j,:) = a(j,:) - factor * a(i,:)
            end if
        end do
    end do

    ! Extract solution
    do i = 1, 3
        x(i) = a(i, 4)
    end do

    ! Print the solution
    print *, 'Solution using Gauss-Jordan elimination:'
    print *, 'x1 = ', x(1)
    print *, 'x2 = ', x(2)
    print *, 'x3 = ', x(3)
end program gauss_jordan
