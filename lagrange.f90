program lagrange
    implicit none
    integer, parameter :: n = 5
    real :: x(n), y(n)
    real :: xp, yp, L, correct,errr
    integer :: i, j
    x = (/1.0, 1.3, 1.5, 1.9, 2.2/)
    do i = 1,n
        y(i) = sin(log(x(i)))
    end do
    xp = 1.1
    yp = 0.0
    do i = 1,n
        L = 1.0
        do j = 1,n
            if(i.NE.j) L = L * ((xp - x(j))/ (x(i)-x(j)))
        end do
        yp = yp + L*y(i)
    end do
    correct = sin(log(xp))
    errr = abs(correct - yp)
    print*, "Our calculated", yp
    print*, "Correct", correct
    print*, "Error", errr
end program
