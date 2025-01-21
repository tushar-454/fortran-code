    K = 2
10  DO 20 I = 3,8,2
        IF(I.EQ.5) GOTO 20
        K = K + I
20  CONTINUE
    K = 2 * K
    PRINT*, "Value of K - ", K
    STOP
    END

! This is a modern Fortran syntax way !

! program main
! 	implicit none
! 	integer :: i, k

! 	k = 2
! 	do i = 3, 8, 2
! 		if(i.eq.5) cycle
! 		k = k + i
! 	end do
! 	k = 2 * k
!   print*, "Value of K - ", K
! end program main
