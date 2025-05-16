! Program to evaluate the integral I = ∫₀^{π/2} e^{sin(x)} dx
! using Simpson's 3/8 rule and Trapezoidal rule, and compare errors

program integral_comparison
  implicit none
  integer :: num_intervals, i
  real :: lower_limit, upper_limit, simpson_result, trap_result, true_value
  real :: error_simpson, error_trap, simpson13_result, error_simpson13

  lower_limit = 0.0
  upper_limit = 1.5707963267948966  ! pi/2
! Main calculation begin
  num_intervals = 6  ! Use n=6 (multiple of 3) for both rules
  simpson_result = simpson_38(lower_limit, upper_limit, num_intervals)
  trap_result = trapezoidal(lower_limit, upper_limit, num_intervals)
  true_value = true_integral(lower_limit, upper_limit)
  error_simpson = abs(simpson_result - true_value)
  error_trap = abs(trap_result - true_value)
  simpson13_result = simpson_13(lower_limit, upper_limit, num_intervals)
  error_simpson13 = abs(simpson13_result - true_value)

  print *, 'Integral I = ∫₀^{π/2} e^{sin(x)} dx'
  print *, 'Simpson''s 3/8 Rule (n=6): ', simpson_result
  print *, 'Trapezoidal Rule   (n=6): ', trap_result
  print *, 'Simpson''s 1/3 Rule (n=6): ', simpson13_result
  print *, 'True Value (n=3000):     ', true_value
  print *, 'Error (Simpson 3/8):     ', error_simpson
  print *, 'Error (Trapezoidal):     ', error_trap
  print *, 'Error (Simpson 1/3):       ', error_simpson13

    ! Function to integrate
contains
  real function f(x)
    real, intent(in) :: x
    f = exp(sin(x))
  end function f

  real function simpson_38(lower_limit, upper_limit, num_intervals)
    integer, intent(in) :: num_intervals
    real, intent(in) :: lower_limit, upper_limit
    real :: h, sum, x
    integer :: i
    if (mod(num_intervals,3) /= 0) then
      print *, 'num_intervals must be a multiple of 3 for Simpson''s 3/8 rule.'
      simpson_38 = 0.0
      return
    end if
    h = (upper_limit - lower_limit) / num_intervals
    sum = f(lower_limit) + f(upper_limit)
    do i = 1, num_intervals-1
      x = lower_limit + i*h
      if (mod(i,3) .eq. 0) then
        sum = sum + 2.0*f(x)
      else
        sum = sum + 3.0*f(x)
      end if
    end do
    simpson_38 = 3.0*h*sum/8.0
  end function simpson_38

  real function trapezoidal(lower_limit, upper_limit, num_intervals)
    integer, intent(in) :: num_intervals
    real, intent(in) :: lower_limit, upper_limit
    real :: h, sum, x
    integer :: i
    h = (upper_limit - lower_limit) / num_intervals
    sum = 0.5 * (f(lower_limit) + f(upper_limit))
    do i = 1, num_intervals-1
      x = lower_limit + i*h
      sum = sum + f(x)
    end do
    trapezoidal = h * sum
  end function trapezoidal

  real function simpson_13(lower_limit, upper_limit, num_intervals)
    integer, intent(in) :: num_intervals
    real, intent(in) :: lower_limit, upper_limit
    real :: h, sum, x
    integer :: i
    if (mod(num_intervals,2) .ne. 0) then
      print *, 'num_intervals must be even for Simpson''s 1/3 rule.'
      simpson_13 = 0.0
      return
    end if
    h = (upper_limit - lower_limit) / num_intervals
    sum = f(lower_limit) + f(upper_limit)
    do i = 1, num_intervals-1
      x = lower_limit + i*h
      if (mod(i,2) .eq. 0) then
        sum = sum + 2.0*f(x)
      else
        sum = sum + 4.0*f(x)
      end if
    end do
    simpson_13 = h*sum/3.0
  end function simpson_13

  real function true_integral(lower_limit, upper_limit)
    real, intent(in) :: lower_limit, upper_limit
    true_integral = simpson_38(lower_limit, upper_limit, 3000)
  end function true_integral

end program integral_comparison
