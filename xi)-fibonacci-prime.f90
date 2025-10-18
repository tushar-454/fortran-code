PROGRAM Fibonacci_Prime
  IMPLICIT NONE
  INTEGER, PARAMETER :: n = 20
  INTEGER :: i
  INTEGER(8), DIMENSION(n) :: fib_numbers
  LOGICAL :: is_prime_number

  ! Generate all Fibonacci numbers first
  fib_numbers(1) = 0
  fib_numbers(2) = 1
  
  DO i = 3, n
    fib_numbers(i) = fib_numbers(i-1) + fib_numbers(i-2)
  END DO

  ! Display results
  PRINT *, "================================================================"
  PRINT *, "First 20 Fibonacci Numbers and Prime Check"
  PRINT *, "================================================================"
  PRINT *, ""
  PRINT *, " No.  |  Fibonacci Number  |  Prime?"
  PRINT *, "------+--------------------+-----------"

  DO i = 1, n
    IF (is_prime_number(fib_numbers(i))) THEN
      PRINT "(I5,A,I18,A)", i, "  | ", fib_numbers(i), "  |  Yes"
    ELSE
      PRINT "(I5,A,I18,A)", i, "  | ", fib_numbers(i), "  |  No"
    END IF
  END DO

  PRINT *, "================================================================"

END PROGRAM Fibonacci_Prime

FUNCTION is_prime_number(num) RESULT(is_prime)
  IMPLICIT NONE
  INTEGER(8), INTENT(IN) :: num
  LOGICAL :: is_prime
  INTEGER(8) :: i

  ! 0 and 1 are not prime
  IF (num .LE. 1) THEN
    is_prime = .FALSE.
    RETURN
  END IF

  ! 2 and 3 are prime
  IF (num .LE. 3) THEN
    is_prime = .TRUE.
    RETURN
  END IF

  ! Even numbers (except 2) are not prime
  IF (MOD(num, 2) .EQ. 0) THEN
    is_prime = .FALSE.
    RETURN
  END IF

  ! Check odd divisors up to sqrt(num)
  DO i = 3, INT(SQRT(REAL(num))), 2
    IF (MOD(num, i) .EQ. 0) THEN
      is_prime = .FALSE.
      RETURN
    END IF
  END DO

  is_prime = .TRUE.

END FUNCTION is_prime_number
