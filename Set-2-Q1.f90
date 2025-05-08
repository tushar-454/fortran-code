program Set2Q1
    implicit none
    integer, parameter :: n = 9
    real :: arr(n)
    real :: mean, diff
    integer :: i
    character(len=4) :: comment

    ! Read 9 distinct numbers
    print *, 'Enter 9 distinct numbers:'
    do i = 1, n
        read *, arr(i)
    end do

    ! Compute arithmetic mean
    mean = sum(arr) / n
    print *, 'Arithmetic mean = ', mean

    ! Print table header
    print *, 'Number', '  Diff', '  Comment'
    print *, '-----------------------------'

    ! Construct and print table
    do i = 1, n
        diff = arr(i) - mean
        if (arr(i) .GE. mean) then
            comment = 'Good'
        else
            comment = 'Bad'
        end if
        print '(F7.2, 2X, F7.2, 2X, A)', arr(i), diff, comment
    end do
end program Set2Q1
