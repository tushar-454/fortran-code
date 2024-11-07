program partial_derivatives

  implicit none

  real*8 :: x, y, h, fx, fy, fxy, fyx, dx, dy, tolerance

  ! Define the function f(x, y)
  real*8 function f(x, y)
    implicit none
    real*8, intent(in) :: x, y
    real*8 :: result

    if (x == 0.0 .and. y == 0.0) then
      result = 0.0
    else
      result = x * y * (x**2 - y**2) / (x**2 + y**2)**2
    end if

    return
  end function f

  ! Calculate partial derivatives numerically
  h = 1.0e-6  ! Step size for numerical differentiation
  tolerance = 1.0e-8

  ! Calculate f_xy(0,0)
  dx = h
  dy = h
  fx = (f(dx, dy) - f(dx, 0.0)) / dy
  fxy = (fx - f(0.0, dy)) / dx

  ! Calculate f_yx(0,0)
  dx = h
  dy = h
  fy = (f(dx, dy) - f(0.0, dy)) / dx
  fyx = (fy - f(dx, 0.0)) / dx

  ! Check for numerical instability
  if (abs(fx) < tolerance .or. abs(fy) < tolerance) then
    print *, "Numerical instability detected. Results may be inaccurate."
  end if

  ! Print the results
  print *, "f_xy(0,0) = ", fxy
  print *, "f_yx(0,0) = ", fyx
  print *, "f_xy(0,0) - f_yx(0,0) = ", fxy - fyx

end program partial_derivatives
