! Gauss-Jordan elimination to solve a 3x3 system of linear equations
program gauss_jordan
    implicit none
    integer :: i, j
    real :: aug(3,4), factor, temp
    real :: roots(3)

    print*, "Enter augmented matrix value:(row wise)"
    read*, ((aug(i, j), j = 1, 4), i = 1, 3)

    do i = 1, 3
        temp = aug(i,i)
        aug(i,:) = aug(i,:) / temp

        do j = 1, 3
            if (j .NE. i) then
                factor = aug(j,i)
                aug(j,:) = aug(j,:) - factor * aug(i,:)
            end if
        end do
    end do

    do i = 1, 3
        roots(i) = aug(i, 4)
    end do

    print *, 'Solution using Gauss-Jordan elimination:'
    print *, 'x = ', roots(1)
    print *, 'y = ', roots(2)
    print *, 'z = ', roots(3)
end program gauss_jordan
