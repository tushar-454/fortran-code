program check_leap_year

implicit none
integer :: year
!Ask the user for input
print *, "Enter a year:"
read *, year20
! Check if the year is leap year
if(mod(year,4) == 0 .and. mod(year,100) /=0 .or. mod(year,400) == 0) then
    print *, year, " is a leap year."
else
    print *, year, " is not a leap year."
end if


end program check_leap_year
