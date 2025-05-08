! Q1. Read a positive integer N >= 50. Print all the odd positive integers from 1 to N
! with three numbers to a line omitting those divisible by 5.
! Determine whether N is prime or not. If N is not prime, print all the divisors.
! Store your result in a file.

program OddNumbersAndPrimeCheck
    implicit none
    integer :: N, i, count, divisorCount, j
    logical :: isPrime
    character(len=100) :: filename
    integer :: outUnit

    ! Prompt user for input
10  print *, 'Enter a positive integer N (N >= 50):'
    read *, N

    ! Check input validity
    if (N < 50) then
        print *, 'N must be greater than or equal to 50.'
        goto 10
    end if

    ! Set output file name
    filename = 'Set-3-Q1-output.txt'
    open(newunit=outUnit, file=filename, status='replace', action='write')

    ! Print odd numbers from 1 to N, omitting those divisible by 5, 3 per line
    print *, 'Odd numbers from 1 to', N, '(excluding multiples of 5):'
    write(outUnit, *) 'Odd numbers from 1 to', N, '(excluding multiples of 5):'
    count = 0
    do i = 1, N, 2
        if (mod(i, 5) .NE. 0) then
            write(*,'(I5)', advance='no') i
            write(outUnit,'(I5)', advance='no') i
            count = count + 1
            if (mod(count, 3) .EQ. 0) then
                print *
                write(outUnit, *)
            end if
        end if
    end do
    ! take new line 
    if (mod(count, 3) .NE. 0) then
        print *
        write(outUnit, *)
    end if

    ! Check if N is prime
    isPrime = .true.
    if (N .LE. 1) then
        isPrime = .false.
    else
        do i = 2, int(sqrt(real(N)))
            if (mod(N, i) .EQ. 0) then
                isPrime = .false.
                exit
            end if
        end do
    end if

    if (isPrime) then
        print *, N, 'is a prime number.'
        write(outUnit, *) N, 'is a prime number.'
    else
        print *, N, 'is not a prime number.'
        write(outUnit, *) N, 'is not a prime number.'
        print *, 'Divisors of', N, ':'
        write(outUnit, *) 'Divisors of', N, ':'
        count = 0
        do i = 1, N
            if (mod(N, i) .EQ. 0) then
                write(*,'(I5)', advance='no') i
                write(outUnit,'(I5)', advance='no') i
                count = count + 1
                if (mod(count, 10) .EQ. 0) then
                    print *
                    write(outUnit, *)
                end if
            end if
        end do
        if (mod(count, 10) .NE. 0) then
            print *
            write(outUnit, *)
        end if
    end if

    close(outUnit)
    print *, 'Results stored in file:', trim(filename)
end program OddNumbersAndPrimeCheck
