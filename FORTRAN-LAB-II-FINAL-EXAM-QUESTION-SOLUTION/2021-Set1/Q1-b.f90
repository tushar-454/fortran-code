PROGRAM compute_terms
  INTEGER :: i, n
  REAL :: term
  EXTERNAL :: general_term

  PRINT *, 'Enter the number of terms:'
  READ *, n

  DO i = 1, n
    CALL general_term(i, term)
    PRINT *, 'Term', i, '=', term
  END DO
END PROGRAM compute_terms

SUBROUTINE general_term(n, term)
  INTEGER :: n
  REAL :: term
  term = (n**2.0) / ((n+1.0)*(n+2.0)) * 4.0**n
END SUBROUTINE general_term
