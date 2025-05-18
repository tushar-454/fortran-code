program integral_comparison
    implicit none
    integer :: interaval,i
    real :: lowLimit, upLimit, sim13Result, sim38Result, trapResult, trueValue,sum,h,x
    real :: errSim38, errTrap

    lowLimit = 4.0
    upLimit = 5.2

    interaval = 6
    sim38Result = 0.0

    h = (upLimit - lowLimit) / interaval
    !3/8
    sum = ALOG(lowLimit) + ALOG(upLimit)
    do i = 1, interaval-1
        x = lowLimit + i * h
        if(mod(i,3).EQ.0) then
            sum = sum + 2.0 * ALOG(x)
        else
            sum = sum + 3.0 * ALOG(x)
        end if
    end do
    sim38Result = (3.0*h*sum)/8.0

   PRINT*,"simpson's 3/8=", sim38Result

    !1/3
   sim13Result = 0.0
   sum = ALOG(lowLimit) + ALOG(upLimit)

       do i = 1, interaval-1
        x = lowLimit + i * h
        if(mod(i,2).EQ.0) then
            sum = sum + 2.0 * ALOG(x)
        else
            sum = sum + 4.0 * ALOG(x)
        end if
    end do
    sim13Result = (h*sum)/3.0

   PRINT*,"simpson's 1/3=", sim13Result

   !trapo
    trapResult = 0.0
   sum = ALOG(lowLimit) + ALOG(upLimit)
       do i = 1, interaval-1
        x = lowLimit + i * h
        sum = sum + 2.0 * ALOG(x)
    end do
    trapResult = (h*sum)/2.0

   PRINT*, "Trapozodial=", trapResult


end program integral_comparison
