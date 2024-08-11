program loop_1_to_100

implicit none
integer :: i

! Loop from 1 to 100

do i = 1, 10
    if(mod(i,2) .ne. 0) then
        print *, i
    end if
end do

end program loop_1_to_100
