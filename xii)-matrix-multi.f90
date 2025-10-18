program Matrix_Multiplication 
  implicit NONE
  integer, parameter :: n = 3
  REAL, DIMENSION(n, n) :: A, B, C
  integer :: i, j, k
  ! Initialize matrices A and B (row-wise)
  A = RESHAPE([1.0, 3.0, 5.0, &
               2.0, 5.0, 7.0, &
               3.0, 8.0, 1.0], [n, n], ORDER=[2,1])
  B = RESHAPE([3.0, 2.0, 15.0, &
               2.0, 3.0, 7.0, &
               13.0, 8.0, 10.0], [n, n], ORDER=[2,1])
  ! Initialize result matrix C to zero
  C = 0.0
  ! Perform matrix multiplication C = A * B
  do i = 1, n
    do j = 1, n
      do k = 1, n
        C(i, j) = C(i, j) + A(i, k) * B(k, j)
      end do
    end do
  end do
  ! shortcut to matrices multiplication -- C = MATMUL(A, B)
  ! Display the result
  print *, "Matrix A:"
  do i = 1, n
    print "(3F8.2)", A(i, :)
  end do
  print *, "Matrix B:"
  do i = 1, n
    print "(3F8.2)", B(i, :)
  end do
  print *, "Resultant Matrix C (A * B):"
  do i = 1, n
    print "(3F8.2)", C(i, :)
  end do
end program Matrix_Multiplication