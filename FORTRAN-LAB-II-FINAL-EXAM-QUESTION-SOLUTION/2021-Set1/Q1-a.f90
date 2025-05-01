SUBROUTINE general_term(n, term)
  INTEGER :: n
  REAL :: term
  term = (n**2.0) / ((n+1.0)*(n+2.0)) * 4.0**n
END SUBROUTINE general_term
