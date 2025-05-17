! Gauss-Jordan elimination to solve a 3x3 system of linear equations
program gauss_jordan
    implicit none
    integer :: i, j
    real :: aug(3,4)
    aug = reshape([1,1,1,3,4,3,8,3,4,4,-2,1],[3,4])
    do i = 1, 3
        aug(i,:) = aug(i,:) / aug(i,i)
        do j = 1, 3
            if (j .NE. i) aug(j,:) = aug(j,:) - (aug(j,i) * aug(i,:))
        end do
    end do
    print *, 'Solution using Gauss-Jordan elimination:'
    print *, 'x = ', aug(1, 4)
    print *, 'y = ', aug(2, 4)
    print *, 'z = ', aug(3, 4)
end program gauss_jordan
