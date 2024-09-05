program numbers_operations
    implicit none
    integer :: n, i, largest, smallest, sum
    integer, allocatable :: numbers(:)

    ! Get the number of elements from the user
    print *, 'Enter the number of elements:'
    read *, n

    ! Allocate memory for the array based on the user's input
    allocate(numbers(n))

    ! Get the numbers from the user
    do i = 1, n
    print *, 'Enter ', i, ' numbers:'
        read *, numbers(i)
    end do

    ! Call the subroutine to find the largest, smallest, and sum
    call subprogram(n, numbers, largest, smallest, sum)

    ! Output the results
    print *, 'The largest number is:', largest
    print *, 'The smallest number is:', smallest
    print *, 'The sum of the numbers is:', sum

    ! Deallocate the array
    deallocate(numbers)

end program numbers_operations

! Subroutine to calculate largest, smallest, and sum of numbers
subroutine subprogram(n, numbers, largest, smallest, sum)
    implicit none
    integer, intent(in) :: n
    integer, intent(in) :: numbers(n)
    integer, intent(out) :: largest, smallest, sum
    integer :: i

    ! Initialize largest and smallest with the first element
    largest = numbers(1)
    smallest = numbers(1)

    ! Initialize sum to 0
    sum = 0

    ! Loop through the numbers to find largest, smallest, and sum
    do i = 1, n
        if (numbers(i) > largest) largest = numbers(i)
        if (numbers(i) < smallest) smallest = numbers(i)
        sum = sum + numbers(i)
    end do

end subroutine subprogram
