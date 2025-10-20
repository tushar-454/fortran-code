PROGRAM MIXED_PARTIALS_NUM
    IMPLICIT NONE
    DOUBLE PRECISION H, K, X, Y, FO, FX, FY, FXY, FYX

    ! Interface for external function F
    INTERFACE
        DOUBLE PRECISION FUNCTION F(X, Y)
        DOUBLE PRECISION X, Y
        END FUNCTION F
    END INTERFACE

    ! We'll respect the order of limits:
    ! f_xy: take x-derivative at x=0 with y=k (h << k), then divide by k.
    ! f_yx: take y-derivative at y=0 with x=h (k << h), then divide by h.

    ! 1) f_xy with h << k
    H = 1.0D-8
    K = 1.0D-4
    Y = K
    FX = (F(+H, Y) - F(-H, Y)) / (2.0D0*H)   ! f_x(0,k)
    FXY = (FX - 0.0D0) / K                   ! since f_x(0,0)=0

    ! 2) f_yx with k << h
    H = 1.0D-4
    K = 1.0D-8
    X = H
    FY = (F(X, +K) - F(X, -K)) / (2.0D0*K)   ! f_y(h,0)
    FYX = (FY - 0.0D0) / H                   ! since f_y(0,0)=0

    WRITE(*,*) 'Mixed partial derivatives at (0,0) via finite differences:'
    WRITE(*,'(A)') 'Using asymmetric steps to respect limit order.'
    WRITE(*,'(A,1X,F12.6)') 'f_xy(0,0) ≈', FXY
    WRITE(*,'(A,1X,F12.6)') 'f_yx(0,0) ≈', FYX
    WRITE(*,*)
    WRITE(*,*) 'Analytic values: f_xy(0,0) = -1,  f_yx(0,0) = +1'

END PROGRAM MIXED_PARTIALS_NUM

DOUBLE PRECISION FUNCTION F(X, Y)
    IMPLICIT NONE
    DOUBLE PRECISION X, Y

    IF (ABS(X) .LT. 1.0D-12 .AND. ABS(Y) .LT. 1.0D-12) THEN
    F = 0.0D0
    ELSE
    F = X * Y * (X**2 - Y**2) / (X**2 + Y**2)
    ENDIF

    RETURN
END