! not understand
program integral_comparison
  implicit none
  integer :: i, n
  real(8) :: a, b, h, x
  real(8) :: trap_result, simpson_result, true_value
  real(8) :: trap_error, simpson_error

  ! Define limits of integration
  a = 0.0d0
  b = 1.57079632679d0  ! pi/2

  ! True value (from high-precision calculation)
  true_value = 2.16395d0

  ! === Trapezoidal Rule ===
  n = 4
  h = (b - a) / n
  trap_result = exp(sin(a)) + exp(sin(b))

  do i = 1, n - 1
     x = a + i * h
     trap_result = trap_result + 2.0d0 * exp(sin(x))
  end do

  trap_result = trap_result * h / 2.0d0

  ! === Simpson's 1/8 Rule ===
  n = 3   ! 3 subintervals for 1/8 rule (must be multiple of 3)
  h = (b - a) / (3.0d0)

  simpson_result = exp(sin(a)) + exp(sin(b))
  simpson_result = simpson_result + 3.0d0 * (exp(sin(a + h)) + exp(sin(a + 2*h)))
  simpson_result = simpson_result + 2.0d0 * exp(sin(a + 3*h))
  simpson_result = simpson_result * (3.0d0 * h / 8.0d0)

  ! === Calculate Errors ===
  trap_error = abs(true_value - trap_result)
  simpson_error = abs(true_value - simpson_result)

  ! === Output Results ===
  print *, '--- Results ---'
  print *, 'True Value         = ', true_value
  print *, 'Trapezoidal Result = ', trap_result
  print *, 'Simpson 1/8 Result = ', simpson_result
  print *, '---------------------'
  print *, 'Trapezoidal Error  = ', trap_error
  print *, 'Simpson 1/8 Error  = ', simpson_error
end program
