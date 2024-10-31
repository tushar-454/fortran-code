!Write a Fortran program which prints each three digit even number N,
!its N^2 and its cube N^3 so that different N's appear on different lines. The
!program should be written so that, the columns of N's is headed by
!number, the column of N^2 's by square and the column of N^3's by cube.

PROGRAM PRINT_EVEN_CUBES
  IMPLICIT NONE
  INTEGER*8 :: N, N2, N3, STARTING, ENDING, INCREMENT

  PRINT*, "Enter the starting value:"
  READ*, STARTING
  PRINT*, "Enter the ending value:"
  READ*, ENDING

  IF((MOD(STARTING,2).NE.0) .OR. (MOD(ENDING,2).NE.0))THEN
    PRINT*, "Starting and Ending value will be EVEN."
    STOP
  END IF

  PRINT*, "Enter the increment value:"
  READ*, INCREMENT

  PRINT '(A,T10,A,T20,A)', "Number", "Square", "Cube"

  DO N = STARTING, ENDING, INCREMENT
    N2 = N * N
    N3 = N * N * N
    PRINT '(I00,I10,I20)', N, N2, N3
  END DO

END PROGRAM PRINT_EVEN_CUBES
