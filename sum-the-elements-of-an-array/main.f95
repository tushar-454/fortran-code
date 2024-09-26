DIMENSION arr(100)
SUM = 0
PRINT *, "Enter the number of elements in the array:"
READ *, N
DO 7 i = 1, N, 1
PRINT *, "Enter the value of elements:",i
READ *, arr(i)
7 CONTINUE
DO 14 i = 1, N,1
SUM = SUM + arr(i)
14 CONTINUE
PRINT *, "The sum of the array elements is", SUM
STOP
END

