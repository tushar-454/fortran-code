PROGRAM series_sum
  IMPLICIT NONE
  INTEGER :: n_terms, i, j
  REAL :: total_sum, harmonic_sum

  ! Initialize variables
  n_terms = 100
  total_sum = 0.0

  ! Calculate the series sum
  DO i = 2, n_terms
    harmonic_sum = 0.0
    DO j = 1, i
      harmonic_sum = harmonic_sum + 1.0 / REAL(j)
    END DO
    total_sum = total_sum + harmonic_sum
  END DO

  PRINT *, "The sum of the series up to", n_terms, "is", total_sum

END PROGRAM series_sum