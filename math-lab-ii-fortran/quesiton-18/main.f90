!Write a subroutine subprogram to find the largest integer and average of the given three integers.

SUBROUTINE FIND_MAX_AVG(A, B, C, MAX_VALUE, AVG_VALUE)
  IMPLICIT NONE
  INTEGER :: A, B, C, MAX_VALUE
  REAL :: AVG_VALUE

  MAX_VALUE = MAX(A, MAX(B, C))

  AVG_VALUE = REAL(A + B + C) / 3.0

END SUBROUTINE FIND_MAX_AVG

PROGRAM MAIN
  IMPLICIT NONE
  INTEGER :: A, B, C, MAX_VALUE
  REAL :: AVG_VALUE
  PRINT*, "Enter the first number:"
  READ *, A
  PRINT*, "Enter the second number:"
  READ *, B
  PRINT*, "Enter the thrid number:"
  READ *, C

  CALL FIND_MAX_AVG(A, B, C, MAX_VALUE, AVG_VALUE)

  PRINT *, "The largest value is:", MAX_VALUE
  PRINT *, "The average value is:", AVG_VALUE

END PROGRAM MAIN
