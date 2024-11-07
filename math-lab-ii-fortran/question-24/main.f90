program newton_raphson
  implicit none
  real(8) :: x, tol, root
  integer :: max_iter

  ! Parameters
  x = 0.5d0                 ! Initial guess (positive starting point)
  tol = 1.0d-6              ! Tolerance for convergence
  max_iter = 100            ! Maximum iterations

  print *, "Finding root of f(x) = cos(x) - x * e^x = 0 using Newton-Raphson method"

  call newton_raphson_subroutine(x, tol, max_iter, root)

  print *, "Root (Newton-Raphson Method): ", root

contains

  ! Function defining f(x) = cos(x) - x * e^x
  real(8) function f(x)
    real(8), intent(in) :: x
    f = cos(x) - x * exp(x)
  end function f

  ! Derivative of f(x), f'(x) = -sin(x) - e^x - x * e^x
  real(8) function df(x)
    real(8), intent(in) :: x
    df = -sin(x) - exp(x) * (1.0d0 + x)
  end function df

  ! Newton-Raphson Method
  subroutine newton_raphson_subroutine(x0, tol, max_iter, root)
    real(8), intent(in) :: x0, tol
    integer, intent(in) :: max_iter
    real(8), intent(out) :: root
    real(8) :: x, fx, dfx
    integer :: iter

    x = x0
    do iter = 1, max_iter
      fx = f(x)
      dfx = df(x)

      if (abs(dfx) < 1.0d-10) then
        print *, "Newton-Raphson method: Derivative near zero; stopping iteration"
        root = x
        return
      end if

      x = x - fx / dfx

      if (abs(fx) < tol) then
        root = x
        return
      end if
    end do

    root = x
    print *, "Newton-Raphson method: Max iterations reached without convergence"
  end subroutine newton_raphson_subroutine

end program newton_raphson
