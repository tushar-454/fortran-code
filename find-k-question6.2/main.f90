! K = 2
! DO 20 I = 3,8,2
! IF(I.EQ.5) GOTO 20
! K = K + 1
! 20 CONTINUE
! K = 2 * K
! PRINT*, "Value of K - ", K
! STOP
! END

program main
	implicit none
	integer :: i, k

	k = 2
	do i = 3,8,2
		if(i.eq.5) then
			k = k * 2
		end if
		k = k + 1
	end do
	
  print*, "Value of K - ", K
end program main