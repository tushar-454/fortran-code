! Program to print Fibonacci sequence as per the question
! (a) Print first 50 Fibonacci numbers
! (b) Print Fibonacci numbers not exceeding 10,000

program fibonacci_sequence
    implicit none
    integer :: i
    integer, parameter :: n_terms = 50
    integer :: fib1, fib2, fib_next

    ! (a) Print first 50 Fibonacci numbers
    print *, 'First 50 Fibonacci numbers:'
    fib1 = 1
    fib2 = 1
    print *, fib1
    print *, fib2
    do i = 3, n_terms
        fib_next = fib1 + fib2
        print *, fib_next
        fib1 = fib2
        fib2 = fib_next
    end do

    ! (b) Print Fibonacci numbers not exceeding 10,000
    print *, ''
    print *, 'Fibonacci numbers not exceeding 10,000:'
    fib1 = 1
    fib2 = 1
    print *, fib1
    print *, fib2
    do while (fib1 + fib2 .LE. 10000)
        fib_next = fib1 + fib2
        print *, fib_next
        fib1 = fib2
        fib2 = fib_next
    end do

end program fibonacci_sequence
