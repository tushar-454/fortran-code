program main
    read*, i,j,k
    call result(i,j,k,min,sum)
    print*, "min=",min
    print*, "sum=",sum
end program main
subroutine result(i,j,k,min,sum)
    sum=i+j+k
    min=i
    if(j.lt.min) min = j
    if(k.lt.min) min = k
    return
end subroutine result
