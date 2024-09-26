program largest_and_sum
    implicit none
    integer :: L, M, N, largest, sum

    ! Input three integers
    print *, 'Enter three integers (L, M, N):'
    read *, L, M, N

    ! Call the subroutine to find the largest and the sum
    call subprogram(L, M, N, largest, sum)

    ! Output the results
    print *, 'The largest number is:', largest
    print *, 'The sum of the numbers is:', sum

end program largest_and_sum

! Subroutine to find the largest and the sum of three integers
subroutine subprogram(L, M, N, largest, sum)
    implicit none
    integer, intent(in) :: L, M, N
    integer, intent(out) :: largest, sum

    ! Calculate the sum of the three numbers
    sum = L + M + N

    ! Determine the largest of the three numbers
    largest = L
    if (M > largest) largest = M
    if (N > largest) largest = N

end subroutine subprogram
