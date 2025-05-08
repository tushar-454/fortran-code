! filepath: g:\Full_Stack_Army\fortran-code\Set-3-Q2.f90
! Q2. Compute and print total pay (basic pay + house rent allowance) based on pay slabs.

program HouseRentAllowance
    implicit none
    real :: basic, hra, total

    ! Input basic pay
    print *, 'Enter the basic pay:'
    read *, basic

    ! Determine HRA percentage based on basic pay
    if (basic .LE. 800) then
        hra = 0.55 * basic
    else if (basic .LE. 1200) then
        hra = 0.50 * basic
    else if (basic .LE. 1650) then
        hra = 0.45 * basic
    else
        hra = 0.40 * basic
    end if

    ! Calculate total pay
    total = basic + hra

    ! Print results
    print *, 'Basic Pay           :', basic
    print *, 'House Rent Allowance:', hra
    print *, 'Total Pay           :', total
end program HouseRentAllowance