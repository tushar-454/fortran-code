PROGRAM partial_sums
  INTEGER :: i
  REAL :: term, sum
  EXTERNAL :: general_term
  OPEN(UNIT=10, FILE='partial-sums.txt', STATUS='REPLACE')

  sum = 0.0

  DO i = 1, 20
    CALL general_term(i, term)
    sum = sum + term
    WRITE(10,*) 'Sum of first', i, 'terms =', sum
  END DO

  CLOSE(10)
END PROGRAM partial_sums

SUBROUTINE general_term(n, term)
  INTEGER :: n
  REAL :: term
  term = (n**2.0) / ((n+1.0)*(n+2.0)) * 4.0**n
END SUBROUTINE general_term

