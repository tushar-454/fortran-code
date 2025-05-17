program integral_comparison
    implicit none
    integer :: interaval,i
    real :: lowLimit, upLimit, sim13Result, sim38Result, trapResult, trueValue,sum,h,x
    real :: errSim38, errTrap

    lowLimit = 1.0
    upLimit = 2.0

    interaval = 6
    sim38Result = 0.0

    h = (upLimit - lowLimit) / interaval
    !3/8
    sum = (1/lowLimit) + (1/upLimit)
    do i = 1, interaval-1
        x = lowLimit + i * h
        if(mod(i,3).EQ.0) then
            sum = sum + 2.0 * (1/x)
        else
            sum = sum + 3.0 * (1/x)
        end if
    end do
    sim38Result = (3.0*h*sum)/8.0

   PRINT '(F10.5)', sim38Result

    !1/3
   sim13Result = 0.0
   sum = (1/lowLimit) + (1/upLimit)

       do i = 1, interaval-1
        x = lowLimit + i * h
        if(mod(i,2).EQ.0) then
            sum = sum + 2.0 * (1/x)
        else
            sum = sum + 4.0 * (1/x)
        end if
    end do
    sim13Result = (h*sum)/3.0

   PRINT '(F10.5)', sim13Result

   !trapo
    trapResult = 0.0
   sum = (1/lowLimit) + (1/upLimit)
       do i = 1, interaval-1
        x = lowLimit + i * h
        sum = sum + 2.0 * (1/x)
    end do
    trapResult = (h*sum)/2.0

   PRINT '(F10.5)', trapResult


end program integral_comparison
