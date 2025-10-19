PROGRAM series_factorial_sum
  IMPLICIT NONE
  INTEGER :: n_terms, i
  REAL(8) :: total_sum, fact

  ! Initialize variables
  n_terms = 25
  total_sum = 1.0d0

  ! Calculate the series sum
  DO i = 2, n_terms
    CALL factorial(i, fact)
    total_sum = total_sum + 1.0d0 / fact
  END DO

  PRINT *, "The sum of the series up to", n_terms, "is", total_sum

END PROGRAM series_factorial_sum

SUBROUTINE factorial(n, fact)
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: n
  INTEGER :: i
  REAL(8), INTENT(OUT) :: fact

  fact = 1.0d0
  DO i = 2, n
    fact = fact * REAL(i, 8)
  END DO
END SUBROUTINE factorial