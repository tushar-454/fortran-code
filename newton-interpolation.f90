  parameter(n=5)
  real x(n), f(n), diff_table(n,n), value, exact_value, newton_poly

  ! Define the x points
x = (/1.0, 1.3, 1.5, 1.9, 2.2/)

  ! Calculate f(x) = sin(log(x)) at each x
  do i = 1, n
     f(i) = sin(log(x(i)))
  end do

  ! Fill the divided difference table
  do i = 1, n
     diff_table(i,1) = f(i)
  end do
  !In the table, first column is filled, we must fill column number 2,3,4...n
  do j = 2, n
     !if a column has k values, next column will have k-1 values
     !  1st column has n values
     !  2nd column has n-1 values  =>  n-2+1
     !  3rd column has n-2 values  =>  n-3+1
     !  4th column has n-3 values  =>  n-4+1
     !  ....
     !  j'th column has ??? values =>  n-j+1
     do i = 1, n-j+1
        diff_table(i,j) = (diff_table(i+1,j-1) - diff_table(i,j-1)) / (x(i+j-1) - x(i))
     end do
  end do

  ! Display divided difference table
  print *, 'Divided Difference Table:'
  do i = 1, n
     print *, (diff_table(i,j), j=1,n-i+1)
  end do

  ! Compute the interpolation polynomial at a new point
  print *, "Enter the value of x to estimate f(x): "
  read *, value

  ! Start with f(x0) = f[0] = diff_table(1,1)
  newton_poly = diff_table(1,1)

  ! Calculate the Newton interpolating polynomial
  do i = 2, n
     term = diff_table(1,i)
     do j = 1, i-1
        term = term * (value - x(j))
     end do
     newton_poly = newton_poly + term
  end do

  ! Exact value
  exact_value = sin(log(value))

  ! Display results
  print *, "Approximated f(x) using Newton's Divided Difference: ", newton_poly
  print *, "Exact f(x): ", exact_value
  print *, "Difference: ", abs(newton_poly - exact_value)

end
